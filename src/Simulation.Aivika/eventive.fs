
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
open System.Collections.Generic

[<Sealed; NoComparison; NoEquality>]
type Eventive<'a> (f: Point -> 'a) =

    member internal x.Fun = f

    static member From (m: Parameter<'a>) = 
        Eventive (fun p -> invokeParameter p.Run m)

    static member From (m: Simulation<'a>) = 
        Eventive (fun p -> invokeSimulation p.Run m)

    static member From (m: Dynamics<'a>) = 
        Eventive (fun p -> invokeDynamics p m)

    static member From (m: Eventive<'a>) = m
    
[<AutoOpen>]
module internal EventiveInvoke =

    let inline invokeEventive p (m: Eventive<'a>) = m.Fun p

module EventiveBuilderImpl =

    let inline returnE a =
        Eventive (fun p -> a)

    let inline bindE m k =
        Eventive (fun p ->
            let a = invokeEventive p m
            let m' = k a
            invokeEventive p m')

    //  letE a k = bindE (returnE a) k
    let inline letE a k =
        Eventive (fun p ->
            let m' = k a
            invokeEventive p m')

    //  delayE k = bindE (returnE ()) k
    let inline delayE k =
        Eventive (fun p ->
            let m' = k ()
            invokeEventive p m')
            
    //  combineE m1 m2 = bindE m1 (fun () -> m2)
    let inline combineE m1 m2 =
        Eventive (fun p ->
            invokeEventive p m1
            invokeEventive p m2)
            
    let zeroE = Eventive (fun p -> ())
    
    let inline forE es k  =
        Eventive (fun p ->
            for e in es do
                invokeEventive p (k e))
                
    let inline whileE pred m =
        Eventive (fun p ->
            while pred () do
                invokeEventive p m)
                
    let inline usingE (a: 'a when 'a :> IDisposable) k =
        Eventive (fun p ->
            try
                invokeEventive p (k a)
            finally
                a.Dispose ())
                
    let inline tryFinallyE m f =
        Eventive (fun p ->
            try
                invokeEventive p m
            finally
                f ())
                
    let inline tryWithE m k =
        Eventive (fun p ->
            try
                invokeEventive p m
            with
            | e -> invokeEventive p (k e))

open EventiveBuilderImpl

[<Sealed>]
type EventiveBuilder() =

    member d.Return (a)  = returnE a
    member d.ReturnFrom (m: Eventive<_>) = m
    member d.Bind (m: Eventive<_>, k) = bindE m k
    member d.Delay (k) = delayE k
    member d.Zero () = zeroE
    member d.Combine (m1: Eventive<_>, m2) = combineE m1 m2
    member d.For (es, k) = forE es k
    member d.While (p, m) = whileE p m
    member d.Using (a, k) = usingE a k
    member d.TryFinally (m, f) = tryFinallyE m f
    member d.TryWith (m, k) = tryWithE m k

[<AutoOpen>]
module EventiveWorkflow =
    let eventive = new EventiveBuilder()

type EventProcessing =
    | CurrentEvents
    | EarlierEvents
    | CurrentEventsOrFromPast
    | EarlierEventsOrFromPast

type EventCancellation =

    abstract Cancelled: Eventive<bool>
    abstract Finished: Eventive<bool>
    abstract Cancel: unit -> Eventive<unit>

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Eventive =

    [<CompiledName ("RunWith")>]
    let runWith processing m =
        Dynamics (fun p ->

            let q = p.Run.EventQueue

            match processing with
            | CurrentEvents -> 
                q.RunSync (p, true)
            | EarlierEvents ->
                q.RunSync (p, false)
            | CurrentEventsOrFromPast ->
                q.Run (p, true)
            | EarlierEventsOrFromPast ->
                q.Run (p, false)
                
            invokeEventive p m)

    [<CompiledName ("Run")>]
    let run m = runWith CurrentEvents m

    [<CompiledName ("RunInStartTime")>]
    let runInStartTime m =
        run m |> Dynamics.runInStartTime

    [<CompiledName ("RunInStopTime")>]
    let runInStopTime m =
        run m |> Dynamics.runInStopTime

    [<CompiledName ("Lift")>]
    let inline lift (x: Eventive<'a>) = (^m: (static member From: Eventive<'a> -> ^m) (x))

    [<CompiledName ("QueueCount")>]
    let queueCount =
        Eventive (fun p -> p.Run.EventQueue.Count)

    [<CompiledName ("Enqueue")>]
    let enqueue t (m: Eventive<_>) =
        Eventive (fun p -> p.Run.EventQueue.Enqueue (t, m.Fun))
    
    [<CompiledName ("EnqueueWithCancellation")>]
    let enqueueWithCancellation t m =
        Eventive (fun p ->

            let cancelled = ref false
            let cancellable = ref true
            let finished = ref false

            let cancel =
                Eventive (fun p ->
                    if !cancellable then
                        cancelled := true)

            Eventive (fun p ->
                cancellable := false
                if not !cancelled then
                    invokeEventive p m
                    finished := true)
            |> enqueue t
            |> invokeEventive p

            { new EventCancellation with

                member x.Cancelled = Eventive (fun p -> !cancelled)
                member x.Finished = Eventive (fun p -> !finished)
                member x.Cancel () = cancel })

    [<CompiledName ("EnqueueWithTimes")>]
    let enqueueWithTimes (xs: #seq<_>) m =
        let rec loop (en: IEnumerator<_>) =
            if en.MoveNext () then
                let x = en.Current
                Eventive (fun p ->
                    invokeEventive p m
                    invokeEventive p (loop en))
                |> enqueue x
            else
                en.Dispose ()
                eventive.Zero ()
        loop (xs.GetEnumerator ())

    [<CompiledName ("EnqueueWithPoints")>]
    let enqueueWithPoints (xs: #seq<Point>) m =
        let rec loop (en: IEnumerator<_>) =
            if en.MoveNext () then
                let x = en.Current
                Eventive (fun p ->
                    invokeEventive x m    // N.B. substitute the time point!
                    invokeEventive p (loop en))
                |> enqueue x.Time
            else
                en.Dispose ()
                eventive.Zero ()
        loop (xs.GetEnumerator ())

    [<CompiledName ("EnqueueWithIntegTimes")>]
    let enqueueWithIntegTimes m =
        Eventive (fun p ->
            let xs = p.IntegPointsFromThis
            enqueueWithPoints xs m
                |> invokeEventive p)

    [<CompiledName ("Concat")>]
    let concat mm = Eventive (fun p ->
        let m = invokeEventive p mm
        invokeEventive p m)
    
    [<CompiledName ("Map")>]
    let map f m = Eventive (fun p ->
        let a = invokeEventive p m
        in f a)
    
    [<CompiledName ("Ap")>]
    let ap mf m = Eventive (fun p ->
        let f = invokeEventive p mf
        let a = invokeEventive p m
        in f a)
    
    [<CompiledName ("Lift2")>]
    let lift2 f m1 m2 = Eventive (fun p ->
        let a1 = invokeEventive p m1
        let a2 = invokeEventive p m2
        in f a1 a2)
    
    [<CompiledName ("Lift3")>]
    let lift3 f m1 m2 m3 = Eventive (fun p ->
        let a1 = invokeEventive p m1
        let a2 = invokeEventive p m2
        let a3 = invokeEventive p m3
        in f a1 a2 a3)
    
    [<CompiledName ("Lift4")>]
    let lift4 f m1 m2 m3 m4 = Eventive (fun p ->
        let a1 = invokeEventive p m1
        let a2 = invokeEventive p m2
        let a3 = invokeEventive p m3
        let a4 = invokeEventive p m4
        in f a1 a2 a3 a4)
    
    [<CompiledName ("Lift5")>]
    let lift5 f m1 m2 m3 m4 m5 = Eventive (fun p ->
        let a1 = invokeEventive p m1
        let a2 = invokeEventive p m2
        let a3 = invokeEventive p m3
        let a4 = invokeEventive p m4
        let a5 = invokeEventive p m5
        in f a1 a2 a3 a4 a5)
        
    [<CompiledName ("Zip")>]
    let zip m1 m2 = Eventive (fun p ->
        let a1 = invokeEventive p m1
        let a2 = invokeEventive p m2
        in (a1, a2)) 
        
    [<CompiledName ("Zip3")>]
    let zip3 m1 m2 m3 = Eventive (fun p ->
        let a1 = invokeEventive p m1
        let a2 = invokeEventive p m2
        let a3 = invokeEventive p m3
        in (a1, a2, a3)) 
    
    [<CompiledName ("OfList")>]
    let ofList (ms: Eventive<_> list) = Eventive (fun p ->
        [ for m in ms -> invokeEventive p m ])
    
    [<CompiledName ("OfArray")>]
    let ofArray (ms: Eventive<_> []) = Eventive (fun p ->
        [| for m in ms -> invokeEventive p m |])
                  
    [<CompiledName ("OfEnumerable")>]
    let ofSeq (ms: #seq<Eventive<_>>) = Eventive (fun p ->
        seq { for m in ms -> invokeEventive p m })

    [<CompiledName ("ToDisposable")>]
    let toDisposable m =
        Eventive (fun p ->
            let r = p.Run
            { new IDisposable with
                member x.Dispose () =
                    let q = r.EventQueue
                    let p = q.Point (r)
                    invokeEventive p m })

    [<CompiledName ("ToLazy")>]
    let toLazy m =
        Eventive (fun p ->
            let r = p.Run
            let force () =
                let q = r.EventQueue
                let p = q.Point (r)
                invokeEventive p m
            lazy (force ()))
   
    [<CompiledName ("Memo")>]
    let memo (m: Eventive<'a>) =

        let dict = ref None
        let dictlock = new obj ()

        let message = 
            "Detected a logical error when the Eventive.memo computation " +
            "was not created safely within the Simulation computation."
        
        in Eventive (fun p ->
        
            match !dict with
            | Some (uniqueId, a) ->

                if uniqueId = p.Run.UniqueId 
                    then a else raise <| InvalidOperationException message

            | None ->

                lock dictlock (fun () ->

                    match !dict with
                    | Some (uniqueId, a) ->

                        if uniqueId = p.Run.UniqueId 
                            then a else raise <| InvalidOperationException message

                    | None ->

                        let a = invokeEventive p m
                        dict := Some (p.Run.UniqueId, a)
                        a))

    [<CompiledName ("Trace")>]
    let trace message (m: Eventive<'a>) =
        Eventive (fun p ->
            printfn "t = %f: %s" p.Time message
            invokeEventive p m)
