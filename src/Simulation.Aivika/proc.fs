
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

type ProcId =
    { mutable Started: bool;
      mutable ReactCont: ContParams<unit> option;
      mutable Interrupted: bool;
      mutable InterruptCont: ContParams<unit> option;
      mutable InterruptTime: Time;
      mutable InterruptVersion: int;
      ContId: ContId }

type ProcId with

    static member Create () =
        Simulation (fun r ->
            let cid = ContId.Create ()
                        |> invokeSimulation r
            { Started = false;
              ReactCont = None;
              Interrupted = false;
              InterruptCont = None;
              InterruptTime = 0.0;
              InterruptVersion = 0;
              ContId = cid })

[<Sealed; NoEquality; NoComparison>]
type Proc<'a> internal (f: ProcId -> Cont<'a>) =

    member internal x.Fun = f

    static member From (m: Parameter<'a>) = 
        Proc (fun pid -> Parameter.lift m)

    static member From (m: Simulation<'a>) = 
        Proc (fun pid -> Simulation.lift m)

    static member From (m: Dynamics<'a>) =
        Proc (fun pid -> Dynamics.lift m)

    static member From (m: Eventive<'a>) =
        Proc (fun pid -> Eventive.lift m)

    static member From (m: Cont<'a>) =
        Proc (fun pid -> m)

    static member From (m: Proc<'a>) = m

[<AutoOpen>]
module internal ProcInvoke =

    let inline invokeProc pid (m: Proc<_>) = m.Fun pid

module ProcBuilderImpl =

    let inline returnP a =
        Proc (fun pid -> cont.Return (a))

    let inline bindP m k =
        Proc (fun pid -> cont.Bind (invokeProc pid m, 
                                    fun a -> invokeProc pid (k a))) 
 
    // delayP k = bindP (returnP ()) k
    let inline delayP k =
        Proc (fun pid -> cont.Delay (fun () -> invokeProc pid (k ())))
            
    // combineP m1 m2 = bindP m1 (fun () -> m2)
    let inline combineP m1 m2 =
        Proc (fun pid -> cont.Combine (invokeProc pid m1, invokeProc pid m2))
            
    let zeroP = Proc (fun pid -> cont.Zero ())
    
    let inline forP es k  =
        Proc (fun pid -> cont.For (es, fun e -> invokeProc pid (k e)))
                
    let inline whileP pred m =
        Proc (fun pid -> cont.While (pred, invokeProc pid m))
                
    let inline usingP (a: 'a when 'a :> IDisposable) k =
        Proc (fun pid -> cont.Using (a, fun a -> invokeProc pid (k a)))
                
    let inline tryFinallyP m f =
        Proc (fun pid -> cont.TryFinally (invokeProc pid m, f))

    let inline tryWithP m k =
        Proc (fun pid -> 
            cont.TryWith (invokeProc pid m,
                          fun e -> invokeProc pid (k e))) 

open ProcBuilderImpl

[<Sealed>]
type ProcBuilder () =

    member d.Return (a)  = returnP a
    member d.ReturnFrom (m: Proc<_>) = m
    member d.Bind (m: Proc<_>, k) = bindP m k
    member d.Delay (k) = delayP k
    member d.Zero () = zeroP
    member d.Combine (m1: Proc<_>, m2) = combineP m1 m2
    member d.For (es, k) = forP es k
    member d.While (p, m) = whileP p m
    member d.Using (a, k) = usingP a k
    member d.TryFinally (m, f) = tryFinallyP m f
    member d.TryWith (m, k) = tryWithP m k

[<AutoOpen>]
module ProcWorkflow =     

    let proc = ProcBuilder ()

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Proc =

    [<CompiledName ("Lift")>]
    let inline lift (x: Proc<'a>) = (^m: (static member From: Proc<'a> -> ^m) (x))

    [<CompiledName ("Concat")>]
    let concat mm =
        Proc (fun pid -> cont {
            let! m = invokeProc pid mm
            return! invokeProc pid m
        })
    
    [<CompiledName ("Map")>]
    let map f m =
        Proc (fun pid -> cont {
            let! a = invokeProc pid m
            return f a
        })
    
    [<CompiledName ("Ap")>]
    let ap mf m =
        Proc (fun pid -> cont {
            let! f = invokeProc pid mf
            let! a = invokeProc pid m
            return f a
        })
    
    [<CompiledName ("Lift2")>]
    let lift2 f m1 m2 =
        Proc (fun pid -> cont {
            let! a1 = invokeProc pid m1
            let! a2 = invokeProc pid m2
            return f a1 a2
        })

    [<CompiledName ("Lift3")>]
    let lift3 f m1 m2 m3 =
        Proc (fun pid -> cont {
            let! a1 = invokeProc pid m1
            let! a2 = invokeProc pid m2
            let! a3 = invokeProc pid m3
            return f a1 a2 a3
        })

    [<CompiledName ("Lift4")>]
    let lift4 f m1 m2 m3 m4 =
        Proc (fun pid -> cont {
            let! a1 = invokeProc pid m1
            let! a2 = invokeProc pid m2
            let! a3 = invokeProc pid m3
            let! a4 = invokeProc pid m4
            return f a1 a2 a3 a4
        })

    [<CompiledName ("Lift5")>]
    let lift5 f m1 m2 m3 m4 m5 =
        Proc (fun pid -> cont {
            let! a1 = invokeProc pid m1
            let! a2 = invokeProc pid m2
            let! a3 = invokeProc pid m3
            let! a4 = invokeProc pid m4
            let! a5 = invokeProc pid m5
            return f a1 a2 a3 a4 a5
        })
 
    [<CompiledName ("Zip")>]
    let zip m1 m2 =
        Proc (fun pid -> cont {
            let! a1 = invokeProc pid m1
            let! a2 = invokeProc pid m2
            return (a1, a2)
        })
    
    [<CompiledName ("Zip3")>]
    let zip3 m1 m2 m3 =
        Proc (fun pid -> cont {
            let! a1 = invokeProc pid m1
            let! a2 = invokeProc pid m2
            let! a3 = invokeProc pid m3
            return (a1, a2, a3)
        })
    
    [<CompiledName ("OfList")>]
    let ofList (ms: Proc<_> list) = 
        Proc (fun pid -> cont {
        
            let xs = ResizeArray<_> ()

            for m in ms do
                let! a = invokeProc pid m
                xs.Add (a)

            return Seq.toList xs
        })
    
    [<CompiledName ("OfArray")>]
    let ofArray (ms: Proc<_> []) = 
        Proc (fun pid -> cont {
        
            let xs = ResizeArray<_> ()

            for m in ms do
                let! a = invokeProc pid m
                xs.Add (a)

            return Seq.toArray xs
        })
        
    [<CompiledName ("OfEnumerable")>]
    let ofSeq (ms: #seq<Proc<_>>) = 
        Proc (fun pid -> cont {
        
            let xs = ResizeArray<_> ()

            for m in ms do
                let! a = invokeProc pid m
                xs.Add (a)

            return xs :> seq<_>
        })

    [<CompiledName ("Hold")>]
    let hold dt =
        Proc (fun pid ->
            Cont (fun c ->
                Eventive (fun p ->
                    if dt < 0.0 then
                        failwithf "Cannot hold the process as the time period is negative."
                    let t = p.Time + dt
                    pid.InterruptCont <- Some c
                    pid.Interrupted <- false
                    pid.InterruptTime <- t
                    let v = pid.InterruptVersion
                    Eventive (fun p ->
                        let v' = pid.InterruptVersion
                        if v = v' then
                            pid.InterruptCont <- None
                            resumeCont c ()
                                |> invokeEventive p)
                        |> Eventive.enqueue t
                        |> invokeEventive p)))

    [<CompiledName ("Interrupt")>]
    let interrupt pid =
        Eventive (fun p ->
            match pid.InterruptCont with
            | None -> ()
            | Some c ->
                pid.InterruptCont <- None
                pid.Interrupted <- true
                pid.InterruptVersion <- 1 + pid.InterruptVersion
                resumeCont c ()
                    |> Eventive.enqueue p.Time
                    |> invokeEventive p)

    [<CompiledName ("IsInterrupted")>]
    let isInterrupted pid =
        Eventive (fun p -> pid.Interrupted)

    [<CompiledName ("Passivate")>]
    let passivate =
        Proc (fun pid ->
            Cont (fun c ->
                Eventive (fun p ->
                    match pid.ReactCont with
                    | None -> 
                        pid.ReactCont <- Some c
                    | Some _ ->
                        failwithf "Cannot passive the process twice.")))

    [<CompiledName ("IsPassivated")>]
    let isPassivated pid =
        Eventive (fun p -> Option.isSome pid.ReactCont)

    [<CompiledName ("Reactivate")>]
    let reactivate pid =
        Eventive (fun p ->
            match pid.ReactCont with
            | None -> ()
            | Some c ->
                pid.ReactCont <- None
                resumeCont c ()
                    |> Eventive.enqueue p.Time
                    |> invokeEventive p)

    /// Define a reaction when the process with the specified identifier is preempted.
    let private preemptedId (pid: ProcId) =
        Eventive (fun p ->
            match pid.InterruptCont with
            | Some c ->
                pid.InterruptCont <- None
                pid.Interrupted <- true
                pid.InterruptVersion <- 1 + pid.InterruptVersion
                let t  = pid.InterruptTime
                let dt = t - p.Time
                let c' = substituteCont c (fun a ->
                            Eventive (fun p ->
                                hold dt
                                    |> invokeProc pid
                                    |> invokeCont c
                                    |> invokeEventive p))
                reenterCont c' ()
                    |> invokeEventive p
            | None ->
                match pid.ReactCont with
                | None -> ()
                | Some c ->
                    let c' = reenterCont c |> substituteCont c
                    pid.ReactCont <- Some c')

    /// Prepares the process identifier before starting the process.
    let private prepareId (pid: ProcId) =
        Eventive (fun p ->

            if pid.Started then
                failwithf "Another process with the specified identifier has been started already."
            else
                pid.Started <- true
        
            let handle e =
                Eventive (fun p ->
                    match e with
                    | ContCancellationInitiating ->
                        if pid.ContId.CancellationActivated then
                            interrupt pid 
                                |> invokeEventive p
                            reactivate pid
                                |> invokeEventive p
                    | ContPreemptionBeginning ->
                        preemptedId pid
                            |> invokeEventive p
                    | ContPreemptionEnding -> ())

            Signal.add handle pid.ContId.Signal
                |> invokeEventive p)

    [<CompiledName ("Id")>]
    let id = Proc (fun pid -> cont.Return (pid))

    [<CompiledName ("CreateId")>]
    let createId = ProcId.Create ()

    [<CompiledName ("UsingId")>]
    let usingId pid m =
        Proc (fun _ -> cont {
            return! Eventive.lift (prepareId pid)
            return! Cont.rerun (invokeProc pid m) pid.ContId
        })

    [<CompiledName ("CancelUsingId")>]
    let cancelUsingId pid = pid.ContId.InitiateCancellation ()
        
    [<CompiledName ("Cancel")>]    
    let cancel<'a> : Proc<'a> = proc {
    
        let! pid = id
        do! cancelUsingId pid |> Eventive.lift
        return! failwithf "The process must be cancelled already."
    }
        
    [<CompiledName ("IsCancelled")>]
    let isCancelled pid = pid.ContId.CancellationInitiated
      
    [<CompiledName ("Cancelling")>]
    let cancelling pid = pid.ContId.CancellationInitiating

    [<CompiledName ("WhenCancelling")>]
    let whenCancelling h =
        Proc (fun pid ->
            cancelling pid 
                |> Signal.add (fun () -> h)
                |> Eventive.lift) 

    [<CompiledName ("BeginPreemption")>]
    let internal beginPreemption pid = pid.ContId.BeginPreemption ()

    [<CompiledName ("EndPreemption")>]
    let internal endPreemption pid = pid.ContId.EndPreemption ()

    [<CompiledName ("PreemptionBeginning")>]
    let preemptionBeginning pid = pid.ContId.PreemptionBeginning

    [<CompiledName ("PreemptionEnding")>]
    let preemptionEnding pid = pid.ContId.PreemptionEnding
            
    [<CompiledName ("RunUsingId")>]
    let runUsingId pid m =
        Eventive (fun p ->

            let cont a  = eventive.Return (a)
            let econt e = raise e
            let ccont e = eventive.Return (e)

            prepareId pid |> invokeEventive p
            
            let m = invokeProc pid m
                
            Cont.run m cont econt ccont pid.ContId
                |> invokeEventive p)

    [<CompiledName ("Run")>]
    let run m =
        Eventive (fun p ->

            let pid = createId |> invokeSimulation p.Run
            runUsingId pid m |> invokeEventive p)

    [<CompiledName ("RunInStartTimeUsingId")>]
    let runInStartTimeUsingId pid m =
        runUsingId pid m |> Eventive.runInStartTime

    [<CompiledName ("RunInStartTime")>]
    let runInStartTime m =
        run m |> Eventive.runInStartTime

    [<CompiledName ("RunInStopTimeUsingId")>]
    let runInStopTimeUsingId pid m =
        runUsingId pid m |> Eventive.runInStopTime

    [<CompiledName ("RunInStopTime")>]
    let runInStopTime m =
        run m |> Eventive.runInStopTime

    [<CompiledName ("SpawnUsingIdWith")>]
    let spawnUsingIdWith cancellation pid m =
        Proc (fun _ -> cont {
            return! Eventive.lift (prepareId pid)
            return! Cont.spawn cancellation (invokeProc pid m) pid.ContId
        })
    
    [<CompiledName ("SpawnWith")>]
    let spawnWith cancellation m = proc {
        let! pid = Simulation.lift createId
        return! spawnUsingIdWith cancellation pid m
    }

    [<CompiledName ("SpawnUsingId")>]
    let spawnUsingId pid m = spawnUsingIdWith CancelTogether pid m

    [<CompiledName ("Spawn")>]
    let spawn m = spawnWith CancelTogether m

    [<CompiledName ("EnqueueUsingId")>]
    let enqueueUsingId t pid m =
        runUsingId pid m |> Eventive.enqueue t

    [<CompiledName ("Enqueue")>]
    let enqueue t m =
        run m |> Eventive.enqueue t

    let private createAndZipIds (xs: list<_>) = simulation {

        let! pids = 
            [ for x in xs do
                yield createId ] |> Simulation.ofList

        return List.zip pids xs
    }
    
    let private prepareZippedIds (xs: list<_>) = eventive {

        for (pid, _) in xs do
            return! prepareId pid
    }

    [<CompiledName ("ParUsingIds")>]
    let parUsingIds xs =
        Proc (fun pid -> cont {

            do! prepareZippedIds xs |> Eventive.lift
            
            let f (pid, m) = (invokeProc pid m, pid.ContId)
            return! Cont.par (List.map f xs)
        })

    [<CompiledName ("Par")>]
    let par ms =
        Proc (fun pid -> cont {

            let! xs = createAndZipIds ms |> Simulation.lift
            do! prepareZippedIds xs |> Eventive.lift

            let f (pid, m) = (invokeProc pid m, pid.ContId)
            return! Cont.par (List.map f xs)
        })

    [<CompiledName ("ParUsingIds_")>]
    let parUsingIds_ xs =
        Proc (fun pid -> cont {

            do! prepareZippedIds xs |> Eventive.lift
            
            let f (pid, m) = (invokeProc pid m, pid.ContId)
            return! Cont.par_ (List.map f xs)
        })

    [<CompiledName ("Par_")>]
    let par_ ms =
        Proc (fun pid -> cont {

            let! xs = createAndZipIds ms |> Simulation.lift
            do! prepareZippedIds xs |> Eventive.lift

            let f (pid, m) = (invokeProc pid m, pid.ContId)
            return! Cont.par_ (List.map f xs)
        })

    [<CompiledName ("Await")>]
    let await s =
        Proc (fun pid -> Cont.await s)

    type private MemoResult<'a> =
        | MemoComputed of 'a
        | MemoError of exn
        | MemoCancelled

    [<CompiledName ("Memo")>]
    let memo m =
        let started  = ref false
        let computed = SignalSource.create |> Simulation.memo
        let value    = ref None
        let result   = 
            proc {
                match Option.get !value with
                | MemoComputed a -> return a
                | MemoError e    -> return! raise e
                | MemoCancelled  -> return! cancel
            }
        proc {
            let! computed = computed |> Simulation.lift
            match !value with
            | Some _ -> return! result
            | None ->
                match !started with
                | true ->
                    do! await (SignalSource.publish computed)
                    return! result
                | false ->
                    started := true
                    let r = ref MemoCancelled
                    let! h = 
                        eventive {
                            value := Some !r
                            do! SignalSource.trigger () computed
                        } |> Eventive.toDisposable
                          |> Eventive.lift 
                    try
                        try 
                            let! a = m    // compute only once!
                            r := MemoComputed a
                        with
                        | e -> r := MemoError e
                    finally
                        h.Dispose ()
                    return! result
        }

    [<CompiledName ("ParZip")>]
    let parZip m1 m2 = proc {

        let! ys = par [map Choice1Of2 m1; map Choice2Of2 m2]
 
        match ys with
        | [Choice1Of2 a; Choice2Of2 b] ->
            return (a, b)
        | _ ->
            return failwithf "Fatal error."
    }

    [<CompiledName ("ParZip3")>]
    let parZip3 m1 m2 m3 = proc {

        let! ys = par [map Choice1Of3 m1; map Choice2Of3 m2; map Choice3Of3 m3]
 
        match ys with
        | [Choice1Of3 a; Choice2Of3 b; Choice3Of3 c] ->
            return (a, b, c)
        | _ ->
            return failwithf "Fatal error."
    }

    [<CompiledName ("Unzip")>]
    let unzip m = 
        let m = memo m
        (map fst m, map snd m)

    [<CompiledName ("TimeoutUsingId")>]
    let timeoutUsingId timeout pid m = proc {
        
        let! s = SignalSource.create |> Simulation.lift
        let! timeoutPid = createId |> Simulation.lift
        
        do! proc {
        
                do! hold timeout
                do! cancelUsingId pid
                        |> Eventive.lift

            } |> spawnUsingIdWith CancelChildAfterParent timeoutPid

        do! proc {
        
                let r = ref None

                let! h1 = cancelUsingId timeoutPid
                            |> Eventive.toDisposable
                            |> Eventive.lift
                
                let! h2 = eventive.Delay (fun () -> SignalSource.trigger !r s)
                            |> Eventive.toDisposable
                            |> Eventive.lift
                
                try
                    try
                        let! a = m
                        r := Some (Choice2Of2 a)
                    with
                    | e -> r := Some (Choice1Of2 e)
                finally
                    h1.Dispose ()
                    h2.Dispose ()
            
            } |> spawnUsingIdWith CancelChildAfterParent pid
            
        let! x = SignalSource.publish s |> await
        
        match x with
        | None -> return None
        | Some (Choice2Of2 a) -> return (Some a)
        | Some (Choice1Of2 e) -> return! raise e
    }                           

    [<CompiledName ("Timeout")>]
    let timeout timeout m = proc {

        let! pid = createId |> Simulation.lift
        return! timeoutUsingId timeout pid m
    }    

    [<CompiledName ("Never")>]
    let never<'a> : Proc<'a> = proc {
        let! pid = createId |> Simulation.lift
        // use the generated identifier so that
        // nobody could reactivate the process,
        // although it can still be canceled
        do! passivate |> usingId pid
        return failwithf "It should never happen."
    }

    [<CompiledName ("RandomUniform")>]
    let randomUniform minimum maximum = proc {

        let! t = Parameter.randomUniform minimum maximum |> Parameter.lift
        do! hold t
        return t
    }

    [<CompiledName ("RandomUniform_")>]
    let randomUniform_ minimum maximum = proc {

        let! t = Parameter.randomUniform minimum maximum |> Parameter.lift
        do! hold t
    }
    
    [<CompiledName ("RandomUniformInt")>]
    let randomUniformInt minimum maximum = proc {

        let! t = Parameter.randomUniformInt minimum maximum |> Parameter.lift
        do! hold (float t)
        return t
    }
    
    [<CompiledName ("RandomUniformInt_")>]
    let randomUniformInt_ minimum maximum = proc {

        let! t = Parameter.randomUniformInt minimum maximum |> Parameter.lift
        do! hold (float t)
    }

    [<CompiledName ("RandomTriangular")>]
    let randomTriangular minimum median maximum = proc {

        let! t = Parameter.randomTriangular minimum median maximum |> Parameter.lift
        do! hold t
        return t
    }

    [<CompiledName ("RandomTriangular_")>]
    let randomTriangular_ minimum median maximum = proc {

        let! t = Parameter.randomTriangular minimum median maximum |> Parameter.lift
        do! hold t
    }
    
    [<CompiledName ("RandomNormal")>]
    let randomNormal mu nu = proc {

        let! t = Parameter.randomNormal mu nu |> Parameter.lift
        if t > 0.0 then do! hold t
        return t
    }
    
    [<CompiledName ("RandomNormal_")>]
    let randomNormal_ mu nu = proc {

        let! t = Parameter.randomNormal mu nu |> Parameter.lift
        if t > 0.0 then do! hold t
    }
    
    [<CompiledName ("RandomExponential")>]
    let randomExponential mu = proc {

        let! t = Parameter.randomExponential mu |> Parameter.lift
        do! hold t
        return t
    }
    
    [<CompiledName ("RandomExponential_")>]
    let randomExponential_ mu = proc {

        let! t = Parameter.randomExponential mu |> Parameter.lift
        do! hold t
    }
    
    [<CompiledName ("RandomErlang")>]
    let randomErlang beta m = proc {

        let! t = Parameter.randomErlang beta m |> Parameter.lift
        do! hold t
        return t
    }
    
    [<CompiledName ("RandomErlang_")>]
    let randomErlang_ beta m = proc {

        let! t = Parameter.randomErlang beta m |> Parameter.lift
        do! hold t
    }
    
    [<CompiledName ("RandomPoisson")>]
    let randomPoisson mu = proc {

        let! t = Parameter.randomPoisson mu |> Parameter.lift
        do! hold (float t)
        return t
    }
    
    [<CompiledName ("RandomPoisson_")>]
    let randomPoisson_ mu = proc {

        let! t = Parameter.randomPoisson mu |> Parameter.lift
        do! hold (float t)
    }
    
    [<CompiledName ("RandomBinomial")>]
    let randomBinomial prob trials = proc {

        let! t = Parameter.randomBinomial prob trials |> Parameter.lift
        do! hold (float t)
        return t
    }

    [<CompiledName ("RandomBinomial_")>]
    let randomBinomial_ prob trials = proc {

        let! t = Parameter.randomBinomial prob trials |> Parameter.lift
        do! hold (float t)
    }

    [<CompiledName ("Trace")>]
    let trace message (m: Proc<'a>) =
        Proc (fun pid ->
            invokeProc pid m
                |> Cont.trace message)
