
// Aivika for .NET
// Copyright (c) 2009-2015  David Sorokin. All rights reserved.
//
// This file is a part of Aivika for .NET
//
// Commercial License Usage
// Licensees holding valid commercial Aivika licenses may use this file in
// accordance with the commercial license agreement provided with the
// Software or, alternatively, in accordance with the terms contained in
// a written agreement between you and David Sorokin, Yoshkar-Ola, Russia. 
// For the further information contact <mailto:david.sorokin@gmail.com>.
//
// GNU General Public License Usage
// Alternatively, this file may be used under the terms of the GNU
// General Public License version 3 or later as published by the Free
// Software Foundation and appearing in the file LICENSE.GPLv3 included in
// the packaging of this file.  Please review the following information to
// ensure the GNU General Public License version 3 requirements will be
// met: http://www.gnu.org/licenses/gpl-3.0.html.

namespace Simulation.Aivika

open System

type ContCancellation = 
        | CancelTogether
        | CancelChildAfterParent
        | CancelParentAfterChild
        | CancelInIsolation

type internal ContEvent =
        | ContCancellationInitiating
        | ContPreemptionBeginning
        | ContPreemptionEnding

[<Sealed>]
type internal ContId private (signalSource: SignalSource<ContEvent>) =

    let mutable cancelInitiated  = false
    let mutable cancelActivated  = false
    let mutable preemptionCount  = ref 0

    let cancelInitiatedComp = 
        Eventive (fun p -> cancelInitiated)

    let cancelInitiateComp =
        Eventive (fun p ->
            if not cancelInitiated then
                cancelInitiated <- true
                cancelActivated <- true
                SignalSource.trigger ContCancellationInitiating signalSource
                    |> invokeEventive p)

    let preemptionBegunComp =
        Eventive (fun p -> !preemptionCount > 0)

    static member Create () =
        Simulation (fun r -> 
            let signalSource = 
                SignalSource.create
                    |> invokeSimulation r
            ContId (signalSource))

    member x.Signal = SignalSource.publish signalSource

    member x.InitiateCancellation () = cancelInitiateComp

    member x.CancellationInitiated = cancelInitiatedComp

    member x.CancellationInitiating = x.Signal |> Signal.filter_ (fun e -> e = ContCancellationInitiating)

    member x.CancellationActivated = cancelActivated

    member x.DeactivateCancellation () = cancelActivated <- false

    member x.BeginPreemption () =
        Eventive (fun p ->
            if not cancelInitiated then
                let n = !preemptionCount
                incr preemptionCount
                if n = 0 then
                    SignalSource.trigger ContPreemptionBeginning signalSource
                        |> invokeEventive p)

    member x.EndPreemption () =
        Eventive (fun p ->
            if not cancelInitiated then
                decr preemptionCount
                let n = !preemptionCount
                if n = 0 then
                    SignalSource.trigger ContPreemptionEnding signalSource
                        |> invokeEventive p)

    member x.PreemptionBegun = preemptionBegunComp

    member x.PreemptionBeginning = x.Signal |> Signal.filter_ (fun e -> e = ContPreemptionBeginning)

    member x.PreemptionEnding = x.Signal |> Signal.filter_ (fun e -> e = ContPreemptionEnding)

    member x.Bind (ys: ContId list) =
        Eventive (fun p ->
            let hs1 = 
                [ for y in ys do
                    yield Signal.subscribe (fun _ -> y.InitiateCancellation ()) x.CancellationInitiating
                            |> invokeEventive p ]
            let hs2 =
                [ for y in ys do
                    yield Signal.subscribe (fun _ -> x.InitiateCancellation ()) y.CancellationInitiating
                            |> invokeEventive p ]
            { new IDisposable with
                member x.Dispose () =
                    for h in hs1 do
                        h.Dispose ()
                    for h in hs2 do
                        h.Dispose () })
                    
    member x.Connect (cancellation, child: ContId) =
        Eventive (fun p ->
            let h1 = match cancellation with
                        | CancelTogether
                        | CancelChildAfterParent ->
                            Signal.subscribe (fun _ -> child.InitiateCancellation ()) x.CancellationInitiating
                                |> invokeEventive p
                        | CancelParentAfterChild
                        | CancelInIsolation ->
                            { new IDisposable with
                                member x.Dispose () = () }
            let h2 = match cancellation with
                        | CancelTogether
                        | CancelParentAfterChild ->
                            Signal.subscribe (fun _ -> x.InitiateCancellation ()) child.CancellationInitiating
                                |> invokeEventive p
                        | CancelChildAfterParent
                        | CancelInIsolation ->
                            { new IDisposable with
                                member x.Dispose () = () }
            { new IDisposable with
                member x.Dispose () =
                    h1.Dispose()
                    h2.Dispose() })

type internal ContParamsAux =
    { ECont: exn -> Eventive<unit>;
      CCont: unit -> Eventive<unit>;
      Id: ContId
    }

type internal ContParams<'a> =
    { Cont: 'a -> Eventive<unit>;
      Aux: ContParamsAux
    }

[<Sealed; NoEquality; NoComparison>]
type internal FrozenCont<'a> (m: Eventive<ContParams<'a> option>) =

    member internal x.Comp = m

[<AutoOpen>]
module ContProtect =

    let protectC f x cont econt =

        let mutable a = Unchecked.defaultof<_>
        let mutable e = null
        
        try
            a <- f x
        with
        | exn -> 
            e <- exn
        
        match e with
        | null -> cont a
        | exn -> econt exn

[<Sealed; NoEquality; NoComparison>]
type Cont<'a> internal (f: ContParams<'a> -> Eventive<unit>) =

    static let contCanceled c = c.Aux.Id.CancellationActivated

    static let cancelCont p c =

        c.Aux.Id.DeactivateCancellation ()
        invokeEventive p (c.Aux.CCont ())
    
    member internal x.Fun = f

    static member From (m: Parameter<'a>) =
        Cont (fun c ->
            Eventive (fun p ->

                if contCanceled c then
                    cancelCont p c
                else

                    protectC (fun r -> invokeParameter r m) p.Run
                        (fun a -> invokeEventive p (c.Cont a))
                        (fun e -> invokeEventive p (c.Aux.ECont e)))) 

    static member From (m: Simulation<'a>) =
        Cont (fun c ->
            Eventive (fun p ->

                if contCanceled c then
                    cancelCont p c
                else

                    protectC (fun r -> invokeSimulation r m) p.Run
                        (fun a -> invokeEventive p (c.Cont a))
                        (fun e -> invokeEventive p (c.Aux.ECont e)))) 

    static member From (m: Dynamics<'a>) =
        Cont (fun c ->
            Eventive (fun p ->
                    
                if contCanceled c then
                    cancelCont p c
                else

                    protectC (fun p -> invokeDynamics p m) p
                        (fun a -> invokeEventive p (c.Cont a))
                        (fun e -> invokeEventive p (c.Aux.ECont e)))) 

    static member From (m: Eventive<'a>) =
        Cont (fun c ->
            Eventive (fun p ->
                    
                if contCanceled c then
                    cancelCont p c
                else

                    protectC (fun p -> invokeEventive p m) p
                        (fun a -> invokeEventive p (c.Cont a))
                        (fun e -> invokeEventive p (c.Aux.ECont e)))) 
        
    static member From (m: Cont<'a>) = m

[<AutoOpen>]
module internal ContInvoke =

    let inline invokeCont c (m: Cont<_>) = m.Fun c

    let inline internal contCanceled c = c.Aux.Id.CancellationActivated

    let inline internal cancelCont p c =

        c.Aux.Id.DeactivateCancellation ()
        invokeEventive p (c.Aux.CCont ())
    
    let internal resumeCont c a =
        Eventive (fun p ->
            if contCanceled c then
                cancelCont p c
            else
                invokeEventive p (c.Cont a)) 

    let internal resumeECont c e =
        Eventive (fun p ->
            if contCanceled c then
                cancelCont p c
            else
                invokeEventive p (c.Aux.ECont e)) 

    let rec internal sleepCont c (a: 'a) =
        Eventive (fun p ->
            let rh = ref None
            let handle e =
                Eventive (fun p ->
                    match !rh with
                    | None -> failwithf "The handler was lost."
                    | Some (h: IDisposable) ->
                        h.Dispose ()
                        match e with
                        | ContCancellationInitiating ->
                            Eventive (fun p ->
                                if contCanceled c then
                                    cancelCont p c)
                                |> Eventive.enqueue p.Time
                                |> invokeEventive p
                        | ContPreemptionBeginning ->
                            failwithf "The computation was already preempted."
                        | ContPreemptionEnding ->
                            reenterCont c a
                                |> Eventive.enqueue p.Time
                                |> invokeEventive p)
            let h = Signal.subscribe handle c.Aux.Id.Signal
                        |> invokeEventive p
            rh := Some h)

    and internal reenterCont c (a: 'a) =
        Eventive (fun p ->
            let f = c.Aux.Id.PreemptionBegun
                        |> invokeEventive p
            if not f then
                Eventive (fun p ->
                    let f = c.Aux.Id.PreemptionBegun
                                |> invokeEventive p
                    if not f then
                        resumeCont c a
                            |> invokeEventive p
                    else
                        sleepCont c a
                            |> invokeEventive p)
                                |> Eventive.enqueue p.Time
                                |> invokeEventive p
            else
                sleepCont c a
                    |> invokeEventive p)

    let internal freezeCont c =
        Eventive (fun p ->
            let rh = ref None
            let rc = ref <| Some c
            let handle e =
                Eventive (fun p ->
                    match !rh with
                    | None -> failwithf "The handler was lost."
                    | Some (h: IDisposable) ->
                        h.Dispose ()
                        match !rc with
                        | None -> ()
                        | Some c ->
                            rc := None
                            Eventive (fun p ->
                                if contCanceled c then
                                    cancelCont p c)
                            |> Eventive.enqueue p.Time
                            |> invokeEventive p)
            let h = Signal.subscribe handle c.Aux.Id.CancellationInitiating
                        |> invokeEventive p
            rh := Some h
            FrozenCont (Eventive (fun p ->
                h.Dispose ()
                let c = !rc
                rc := None
                c)))

    let internal freezeContReentering c (a: 'a) (m: Eventive<unit>) =
        Eventive (fun p ->
            let rh = ref None
            let rc = ref <| Some c
            let handle e =
                Eventive (fun p ->
                    match !rh with
                    | None -> failwithf "The handler was lost."
                    | Some (h: IDisposable) ->
                        h.Dispose ()
                        match !rc with
                        | None -> ()
                        | Some c ->
                            rc := None
                            Eventive (fun p ->
                                if contCanceled c then
                                    cancelCont p c)
                            |> Eventive.enqueue p.Time
                            |> invokeEventive p)
            let h = Signal.subscribe handle c.Aux.Id.CancellationInitiating
                        |> invokeEventive p
            rh := Some h
            FrozenCont (Eventive (fun p ->
                h.Dispose ()
                let c = !rc
                rc := None
                match c with
                | None as z -> z
                | Some c as z ->
                    let f = c.Aux.Id.PreemptionBegun
                                |> invokeEventive p
                    if not f then z
                    else
                        let c = { c with Cont = fun a -> m }
                        sleepCont c a |> invokeEventive p
                        None)))

    let inline internal unfreezeCont (c: FrozenCont<'a>) = c.Comp

    let internal substituteCont c f = { c with Cont = f }

module ContBuilderImpl =

    let inline returnC a = 
        Cont (fun c -> 
            Eventive (fun p ->
            
                if contCanceled c then
                    cancelCont p c
                else
                    invokeEventive p (c.Cont a)))
 
    let inline bindC m k = 
        Cont (fun c ->
            Eventive (fun p ->

                if contCanceled c then
                    cancelCont p c
                else

                    let cont a = protectC k a (invokeCont c) c.Aux.ECont
                    let c' = { Cont = cont; Aux = c.Aux }

                    invokeCont c' m
                        |> invokeEventive p)) 

    // letC a k = bindC (returnC a) k
    let inline letC a k = 
        Cont (fun c ->
            Eventive (fun p ->

                if contCanceled c then
                    cancelCont p c

                else
                    protectC k a (invokeCont c) c.Aux.ECont
                        |> invokeEventive p))

    //  delayC k = bindC (returnC ()) k
    let inline delayC k = letC () k

    //  callC k a = bindC (returnC a) k        
    let inline callC k a = letC a k

    let zeroC = returnC ()
    
    let inline tryFinallyC m f = 
        Cont (fun c ->
            Eventive (fun p ->

                if contCanceled c then
                    cancelCont p c

                else

                    let cont a = protectC f () (fun () -> c.Cont a) c.Aux.ECont
                    let econt e = protectC f () (fun () -> c.Aux.ECont e) c.Aux.ECont
                    let ccont e = protectC f () (fun () -> c.Aux.CCont e) (fun _ -> c.Aux.CCont e)

                    let c' = { Cont = cont;
                               Aux = { c.Aux with ECont = econt;
                                                  CCont = ccont } }

                    invokeCont c' m
                        |> invokeEventive p))
    
    let inline tryWithC m k =
        Cont (fun c ->
            Eventive (fun p ->

                if contCanceled c then
                    cancelCont p c

                else

                    let econt e = 
                        callC k e 
                            |> invokeCont c

                    let c' = { c with Aux = { c.Aux with ECont = econt } }
                    
                    invokeCont c' m 
                        |> invokeEventive p))

    let inline usingC (r: 'a :> IDisposable) k =  
        tryFinallyC (callC k r) (fun () -> r.Dispose())

    let rec whileC p m =
        if p () then 
            bindC m (fun () -> whileC p m) 
        else 
            zeroC
            
    let rec forC (es: seq<_>) m =
        usingC (es.GetEnumerator()) (fun ie ->
            whileC
                (fun () -> ie.MoveNext())
                (delayC (fun () -> m ie.Current)))

    let inline combineC m1 m2 = 
        bindC m1 (fun () -> m2)

open ContBuilderImpl

[<Sealed>]
type ContBuilder() =

    member d.Return (a)  = returnC a
    member d.ReturnFrom (m: Cont<_>) = m
    member d.Bind (m: Cont<_>, k) = bindC m k
    member d.Delay (k) = delayC k
    member d.Zero () = zeroC
    member d.Combine (m1: Cont<_>, m2) = combineC m1 m2
    member d.For (es, k) = forC es k
    member d.While (p, m) = whileC p m
    member d.Using (a, k) = usingC a k
    member d.TryFinally (m, f) = tryFinallyC m f
    member d.TryWith (m, k) = tryWithC m k

[<AutoOpen>]
module internal ContWorkflow =     

    let cont = ContBuilder ()

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module internal Cont =

    [<CompiledName ("Run")>]
    let run m cont econt ccont cid =
        m |> invokeCont { Cont = cont;
                          Aux = { ECont = econt;
                                  CCont = ccont;
                                  Id = cid } }

    [<CompiledName ("Lift")>]
    let inline lift (x: Cont<'a>) = (^m: (static member From: Cont<'a> -> ^m) (x))

    [<CompiledName ("Concat")>]
    let concat (mm: Cont<Cont<_>>) = cont {
        let! (m: Cont<_>) = mm
        return! m
    }
    
    [<CompiledName ("Map")>]
    let map f m = cont {
        let! a = m
        return f a
    }
    
    [<CompiledName ("Ap")>]
    let ap mf m = cont {
        let! f = mf
        let! a = m
        return f a
    } 
    
    [<CompiledName ("Lift2")>]
    let lift2 f m1 m2 = cont {
        let! a1 = m1
        let! a2 = m2
        return f a1 a2
    } 

    [<CompiledName ("Lift3")>]
    let lift3 f m1 m2 m3 = cont {
        let! a1 = m1
        let! a2 = m2
        let! a3 = m3
        return f a1 a2 a3
    } 
    
    [<CompiledName ("Lift4")>]
    let lift4 f m1 m2 m3 m4 = cont {
        let! a1 = m1
        let! a2 = m2
        let! a3 = m3
        let! a4 = m4
        return f a1 a2 a3 a4
    } 
    
    [<CompiledName ("Lift5")>]
    let lift5 f m1 m2 m3 m4 m5 = cont {
        let! a1 = m1
        let! a2 = m2
        let! a3 = m3
        let! a4 = m4
        let! a5 = m5
        return f a1 a2 a3 a4 a5
    } 
        
    [<CompiledName ("Zip")>]
    let zip m1 m2 = cont {
        let! a1 = m1
        let! a2 = m2
        return (a1, a2)
    } 
        
    [<CompiledName ("Zip3")>]
    let zip3 m1 m2 m3 = cont {
        let! a1 = m1
        let! a2 = m2
        let! a3 = m3
        return (a1, a2, a3)
    } 
    
    [<CompiledName ("OfList")>]
    let ofList (ms: Cont<_> list) = cont {
        
        let xs = ResizeArray<_> ()

        for m in ms do
            let! a = m
            xs.Add (a)

        return Seq.toList xs
    }
    
    [<CompiledName ("OfArray")>]
    let ofArray (ms: Cont<_> []) = cont {
        
        let xs = ResizeArray<_> ()

        for m in ms do
            let! a = m
            xs.Add (a)

        return Seq.toArray xs
    }
                  
    [<CompiledName ("OfEnumerable")>]
    let ofSeq (ms: #seq<Cont<_>>) = cont {
        
        let xs = ResizeArray<_> ()

        for m in ms do
            let! a = m
            xs.Add (a)

        return xs :> seq<_>
    }

    [<CompiledName ("Par")>]
    let par xs =
        Cont (fun c ->
            Eventive (fun p ->
                let n = List.length xs
                let worker () =
                    let results = Array.zeroCreate n
                    let counter = ref 0
                    let catch   = ref null
                    let hs = c.Aux.Id.Bind (List.map snd xs)
                                |> invokeEventive p
                    let propagate =
                        Eventive (fun p ->
                            if n = !counter then
                                hs.Dispose ()
                                if contCanceled c then
                                    cancelCont p c
                                else if !catch = null then
                                    resumeCont c (results |> Array.toList)
                                        |> invokeEventive p
                                else
                                    resumeECont c !catch
                                        |> invokeEventive p)
                    let cont i a =
                        Eventive (fun p ->
                            incr counter
                            results.[i] <- a
                            invokeEventive p propagate)
                    let econt e =
                        Eventive (fun p ->
                            incr counter
                            // ignore the next exception
                            if !catch = null then catch := e
                            invokeEventive p propagate)
                    let ccont e =
                        Eventive (fun p ->
                            incr counter
                            // the main computation was automatically canceled
                            invokeEventive p propagate)
                    for ((m, cid), i) in List.zip xs [0 .. n-1] do
                        run m (cont i) econt ccont cid
                            |> invokeEventive p
                if contCanceled c then
                    cancelCont p c
                elif n = 0 then
                    invokeEventive p (c.Cont [])
                else
                    worker ()))

    [<CompiledName ("Par_")>]
    let par_ xs =
        Cont (fun c ->
            Eventive (fun p ->
                let n = List.length xs
                let worker () =
                    let counter = ref 0
                    let catch   = ref null
                    let hs = c.Aux.Id.Bind (List.map snd xs)
                                |> invokeEventive p
                    let propagate =
                        Eventive (fun p ->
                            if n = !counter then
                                hs.Dispose ()
                                if contCanceled c then
                                    cancelCont p c
                                else if !catch = null then
                                    resumeCont c ()
                                        |> invokeEventive p
                                else
                                    resumeECont c !catch
                                        |> invokeEventive p)
                    let cont i a =
                        Eventive (fun p ->
                            incr counter
                            // ignore the result
                            invokeEventive p propagate)
                    let econt e =
                        Eventive (fun p ->
                            incr counter
                            // ignore the next exception
                            if !catch = null then catch := e
                            invokeEventive p propagate)
                    let ccont e =
                        Eventive (fun p ->
                            incr counter
                            // the main computation was automatically canceled
                            invokeEventive p propagate)
                    for ((m, cid), i) in List.zip xs [0 .. n-1] do
                        run m (cont i) econt ccont cid
                            |> invokeEventive p
                if contCanceled c then
                    cancelCont p c
                elif n = 0 then
                    invokeEventive p (c.Cont ())
                else
                    worker ()))    
                    
    [<CompiledName ("ReRun")>]
    let rerun m cid =
        Cont (fun c ->
            Eventive (fun p ->
                let worker () =
                    let hs = c.Aux.Id.Bind [cid]
                                |> invokeEventive p
                    let cont a =
                        Eventive (fun p ->
                            hs.Dispose ()    // unbind the token
                            invokeEventive p (resumeCont c a))
                    let econt e =
                        Eventive (fun p ->
                            hs.Dispose ()    // unbind the token
                            invokeEventive p (resumeECont c e))
                    let ccont e =
                        Eventive (fun p ->
                            hs.Dispose ()    // unbind the token
                            cancelCont p c)
                    run m cont econt ccont cid
                        |> invokeEventive p
                if contCanceled c then
                    cancelCont p c
                else
                    worker ()))    
                    
    [<CompiledName ("Spawn")>]
    let spawn cancellation m cid : Cont<unit> =
        Cont (fun c ->
            Eventive (fun p ->
                let worker () =
                    let hs = c.Aux.Id.Connect (cancellation, cid)
                                |> invokeEventive p
                    let cont () =
                        Eventive (fun p ->
                            hs.Dispose ()    // unbind the token
                            (* do nothing and it will finish the child computation *))
                    let econt e =
                        Eventive (fun p ->
                            hs.Dispose ()    // unbind the token
                            raise e)     // this is all we can do
                    let ccont e =
                        Eventive (fun p ->
                            hs.Dispose ()    // unbind the token
                            (* do nothing and it will finish the child computation *))
                    run m cont econt ccont cid
                        |> Eventive.enqueue p.Time
                        |> invokeEventive p
                    resumeCont c ()
                        |> invokeEventive p
                if contCanceled c then
                    cancelCont p c
                else
                    worker ()))    

    [<CompiledName ("Await")>]
    let await s =
        Cont (fun c ->
            Eventive (fun p ->
                let c = freezeCont c |> invokeEventive p
                let r = ref None
                let handle a =
                    Eventive (fun p ->
                        match !r with
                        | None ->
                            failwithf "The signal was lost."
                        | Some (h: IDisposable) ->
                            h.Dispose ()
                            let c = unfreezeCont c |> invokeEventive p
                            match c with
                            | None -> ()
                            | Some c ->
                                reenterCont c a
                                    |> invokeEventive p)
                let h = Signal.subscribe handle s
                            |> invokeEventive p
                r := Some h))

    [<CompiledName ("Trace")>]
    let trace message (m: Cont<'a>) =
        Cont (fun c ->
            Eventive (fun p ->
                if contCanceled c then
                    cancelCont p c
                else
                    printfn "t = %f: %s" p.Time message
                    invokeEventive p (invokeCont c m)))
