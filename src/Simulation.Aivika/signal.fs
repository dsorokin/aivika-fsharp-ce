
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

[<AbstractClass; NoEquality; NoComparison>]
type Signal<'a> () =

    abstract Subscribe: ('a -> Eventive<unit>) -> Eventive<IDisposable> 
    
[<AutoOpen>]
module SignalExtensions =

    type Signal<'a> with

        member x.Add (h) =
            Eventive (fun p ->
                invokeEventive p (x.Subscribe (h)) |> ignore) 

type SignalHandler<'a> (f: 'a -> Eventive<unit>) =

    member x.Fun = f 

[<Sealed>]
type SignalSource<'a> private () =

    let mutable handlers: SignalHandler<'a> list = []

    let rec unsubscribe h xs acc = 
        match xs with
            | [] -> List.rev acc
            | (x :: xs) -> 
                if x = h then 
                    (List.rev acc) @ xs
                else
                    unsubscribe h xs (x :: acc)
    
    let signal =
        { new Signal<_> () with
            member x.Subscribe (h) =
                Eventive (fun p ->
                    let h' = SignalHandler (h)
                    handlers <- h' :: handlers
                    { new IDisposable with
                        member x.Dispose () =
                            handlers <- unsubscribe h' handlers [] })}

    static member Create () = Simulation (fun r -> SignalSource<'a> ())

    member x.Publish = signal

    member x.Trigger (a) =
        Eventive (fun p ->
            let hs = handlers
            for h in hs do
                invokeEventive p (h.Fun a))

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Signal =

    [<CompiledName ("Add")>]
    let add h (s: Signal<_>) = s.Add (h)
    
    [<CompiledName ("Subscribe")>]
    let subscribe h (s: Signal<_>) = s.Subscribe (h)

    [<CompiledName ("Map")>]
    let map f (s: Signal<_>) =
        { new Signal<_> () with
            member x.Subscribe (h) =
                s.Subscribe (f >> h) }

    [<CompiledName ("MapC")>]
    let mapc (f: 'a -> Eventive<'b>) (s: Signal<'a>) =
        { new Signal<_> () with
            member x.Subscribe (h) =
                s.Subscribe (fun a -> eventive.Bind(f a, h)) }

    [<CompiledName ("Ap")>]
    let ap (mf: Eventive<'a -> 'b>) (s: Signal<'a>) =
        { new Signal<_> () with
            member x.Subscribe (h) =
                s.Subscribe (fun a -> eventive.Bind(mf, fun f -> h (f a))) }

    [<CompiledName ("Filter")>]
    let filter pred (s: Signal<_>) =
        { new Signal<_> () with
            member x.Subscribe (h) =
                s.Subscribe (fun a -> 
                    if pred a then h a else eventive.Zero ()) }

    [<CompiledName ("Filter_")>]
    let filter_ pred (s: Signal<_>) =
        { new Signal<_> () with
            member x.Subscribe (h) =
                s.Subscribe (fun a -> 
                    if pred a then h () else eventive.Zero ()) }

    [<CompiledName ("FilterC")>]
    let filterc (pred: 'a -> Eventive<bool>) (s: Signal<'a>) =
        { new Signal<_> () with
            member x.Subscribe (h) =
                s.Subscribe (fun a ->
                    eventive.Bind (pred a, fun x ->
                        if x then h a else eventive.Zero ())) }

    [<CompiledName ("FilterC_")>]
    let filterc_ (pred: 'a -> Eventive<bool>) (s: Signal<'a>) =
        { new Signal<_> () with
            member x.Subscribe (h) =
                s.Subscribe (fun a ->
                    eventive.Bind (pred a, fun x ->
                        if x then h () else eventive.Zero ())) }

    [<CompiledName ("Empty")>]
    let empty<'a> =
        { new Signal<'a> () with
            member x.Subscribe (h) =
                Eventive (fun p ->
                    { new IDisposable with
                        member x.Dispose () = () })}

    [<CompiledName ("Merge")>]
    let merge (s1: Signal<'a>) (s2: Signal<'a>) =
        { new Signal<_> () with
            member x.Subscribe (h) =
                Eventive (fun p ->

                    let x1 = s1.Subscribe (h) |> invokeEventive p
                    let x2 = s2.Subscribe (h) |> invokeEventive p

                    { new IDisposable with
                        member x.Dispose () =

                            x1.Dispose ()
                            x2.Dispose () })}

    [<CompiledName ("Concat")>]
    let concat xs =
        List.fold merge empty xs

    let private triggerWithCurrentTime (s: SignalSource<_>) =
        Eventive (fun p ->
            invokeEventive p (s.Trigger (p.Time)))
    
    [<CompiledName ("InTimes")>]
    let inTimes (xs: #seq<_>) =
        Eventive (fun p ->
            let s = SignalSource<_>.Create () |>
                        invokeSimulation p.Run
            triggerWithCurrentTime s
                |> Eventive.enqueueWithTimes xs
                |> invokeEventive p
            s.Publish)

    [<CompiledName ("InIntegTimes")>]
    let inIntegTimes =
        Eventive (fun p ->
            let s = SignalSource<_>.Create () |> 
                        invokeSimulation p.Run
            triggerWithCurrentTime s
                |> Eventive.enqueueWithIntegTimes
                |> invokeEventive p
            s.Publish)

    [<CompiledName ("InStartTime")>]
    let inStartTime =
        Eventive (fun p ->
            let s = SignalSource<_>.Create () |> 
                        invokeSimulation p.Run
            let t = Parameter.starttime |>
                        invokeParameter p.Run
            triggerWithCurrentTime s
                |> Eventive.enqueue t
                |> invokeEventive p
            s.Publish)
    
    [<CompiledName ("InStopTime")>]
    let inStopTime =
        Eventive (fun p ->
            let s = SignalSource<_>.Create () |> 
                        invokeSimulation p.Run
            let t = Parameter.stoptime |>
                        invokeParameter p.Run
            triggerWithCurrentTime s
                |> Eventive.enqueue t
                |> invokeEventive p
            s.Publish)

    [<CompiledName ("ToArrival")>]
    let toArrival (s: Signal<_>) =
        { new Signal<_>() with
            member x.Subscribe (h) = eventive {
                let r = ref None
                return! s.Subscribe (fun a ->
                    Eventive (fun p ->
                        let t0 = !r
                        let t  = p.Time
                        r := Some t
                        match t0 with
                        | None ->
                            h { Value = a; Time = t; Delay = None }
                                |> invokeEventive p
                        | Some t0 ->
                            h { Value = a; Time = t; Delay = Some (t - t0) }
                                |> invokeEventive p))
            }
        }
                
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module SignalSource =

    [<CompiledName ("Create")>]
    let create<'a> = SignalSource<'a>.Create ()
    
    [<CompiledName ("Publish")>]
    let publish (s: SignalSource<_>) = s.Publish
    
    [<CompiledName ("Trigger")>]
    let trigger a (s: SignalSource<_>) = s.Trigger (a)
    
[<Sealed>]
type SignalHistory<'a> private (ts: ResizeArray<_>, xs: ResizeArray<_>) =

    let read =
        Eventive (fun p -> (ts.ToArray (), xs.ToArray ()))

    static member Create (s: Signal<'a>) =
        Eventive (fun p ->

            let ts = ResizeArray<_> ()
            let xs = ResizeArray<_> ()

            let h a = 
                Eventive (fun p ->

                    ts.Add (p.Time)
                    xs.Add (a))

            Signal.add h s
                |> invokeEventive p

            SignalHistory<_> (ts, xs))

    member x.Read () = read

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module SignalHistory =

    [<CompiledName ("Create")>]
    let create s = SignalHistory<_>.Create (s)
    
    [<CompiledName ("Read")>]
    let read (h: SignalHistory<'a>) = h.Read ()

[<NoEquality; NoComparison>]
type ISignalable<'a> =

    abstract Read: unit -> Eventive<'a>

    abstract Changed_: Signal<unit>

[<NoEquality; NoComparison>]
type Signalable<'a> (read, changed_) =

    interface ISignalable<'a> with

        member x.Read () = read
        member x.Changed_ = changed_

[<AutoOpen>]
module SignalableExtensions =

    type ISignalable<'a> with

        member x.Changed =
            x.Changed_ |> Signal.mapc (fun () -> x.Read ())

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Signalable =

    [<CompiledName ("Read")>]
    let read (m: ISignalable<_>) = m.Read ()

    [<CompiledName ("Changed_")>]
    let changed_ (m: ISignalable<_>) = m.Changed_

    [<CompiledName ("Changed")>]
    let changed (m: ISignalable<_>) = m.Changed

    [<CompiledName ("Map")>]
    let map f (m: ISignalable<_>) = 
        { new ISignalable<_> with
            member x.Read () = Eventive.map f (read m)
            member x.Changed_ = changed_ m }

type PredefinedSignalSet =
    { SignalInIntegTimes: Signal<float>;
      SignalInStartTime: Signal<float>;
      SignalInStopTime: Signal<float> }

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module PredefinedSignalSet =

    [<CompiledName ("Create")>]
    let create = eventive {
            let! s1 = Signal.inIntegTimes
            let! s2 = Signal.inStartTime
            let! s3 = Signal.inStopTime
            return { SignalInIntegTimes = s1;
                     SignalInStartTime  = s2;
                     SignalInStopTime   = s3 } }
