
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

#nowarn "40"

namespace Simulation.Aivika

open System
open System.Collections.Generic

type internal WireItem<'a> =
    | WireNil
    | WireCons of 'a * Wire<'a>

and [<Sealed; NoEquality; NoComparison>] Wire<'a> internal (p: Eventive<WireItem<'a>>) =

    member internal x.Eventive = p

    static member From (m: Parameter<unit>): Wire<'a> =
        Wire (Eventive (fun p ->
            invokeParameter p.Run m
            WireNil))

    static member From (m: Simulation<unit>): Wire<'a> =
        Wire (Eventive (fun p ->
            invokeSimulation p.Run m
            WireNil))

    static member From (m: Dynamics<unit>): Wire<'a> =
        Wire (Eventive (fun p ->
            invokeDynamics p m
            WireNil))

    static member From (m: Eventive<unit>): Wire<'a> =
        Wire (Eventive (fun p ->
            invokeEventive p m
            WireNil))

    static member From (m: Wire<'a>) = m

[<AutoOpen>]
module internal WireInvoke =

    let inline invokeWire (m: Wire<'a>) = m.Eventive

module WireBuilderImpl =

    let zeroW<'a> =
        Wire (Eventive (fun p -> WireNil: WireItem<'a>))

    let inline yieldW a =
        Wire (Eventive (fun p -> WireCons (a, zeroW)))

    let returnW (u: 'u): Wire<'a> =
        match box u with
        | :? unit -> zeroW
        | _ -> failwithf "Only the unit type is supported."

    let inline bindW m k =
        Wire (eventive.Bind (m, fun a -> invokeWire (k a)))

    // delayW k = bindW (returnE ()) k
    let inline delayW k =
        Wire (eventive.Delay (fun () -> invokeWire (k ())))

    let rec combineW m1 m2 =
        Wire (Eventive (fun p ->
          match invokeEventive p (invokeWire m1) with
          | WireNil ->
                invokeEventive p (invokeWire m2)
          | WireCons (a, m1') ->
                WireCons (a, combineW m1' m2)))
                
    let rec whileW pred m =
        Wire (Eventive (fun p ->
            if pred () then
                invokeEventive p (invokeWire (combineW m (whileW pred m)))
            else
                WireNil))

    let inline usingW (a: 'a when 'a :> IDisposable) k =
        Wire (eventive.Using (a, fun a -> invokeWire (k a)))

    let inline tryFinallyW m f =
        Wire (eventive.TryFinally (invokeWire m, f))

    let inline tryWithW m k =
        Wire (eventive.TryWith (invokeWire m, fun e -> invokeWire (k e)))

    let rec forW es k =
        Wire (Eventive (fun p ->
            let e = invokeEventive p (invokeWire es)
            match e with
            | WireNil -> 
                WireNil
            | WireCons (a, es') ->
                invokeEventive p (invokeWire (combineW (k a) (forW es' k)))))

    let forSeqW (es: seq<_>) k =
        usingW (es.GetEnumerator()) <| fun ie ->
            let rec loop (ie: IEnumerator<_>) = 
                Wire (Eventive (fun p ->
                    if ie.MoveNext () then
                        let a = ie.Current
                        invokeEventive p (invokeWire (combineW (k a) (loop ie)))
                    else
                        WireNil))
            loop ie

open WireBuilderImpl

[<Sealed>]
type WireBuilder () =

    member x.Yield (a) = yieldW a
    member x.YieldFrom (m: Wire<_>) = m
    member x.Return (u) = returnW u
    member x.ReturnFrom (m: Eventive<_>) = bindW m (fun () -> zeroW)
    member x.Bind (m: Eventive<_>, k) = bindW m k
    member x.Delay (k) = delayW k
    member x.Zero () = zeroW
    member x.Combine (m1, m2) = combineW m1 m2
    member x.For (es: Wire<_>, k) = forW es k
    member x.For (es: seq<_>, k) = forSeqW es k
    member x.While (p, m) = whileW p m
    member x.Using (a, k) = usingW a k
    member x.TryFinally (m, f) = tryFinallyW m f
    member x.TryWith (m, k) = tryWithW m k

[<AutoOpen>]
module WireWorkflow =     

    let wire = WireBuilder ()

[<AutoOpen>]
module EventiveBuilderExtensions =

    type EventiveBuilder with

        member x.For (es: Wire<'a>, k: 'a -> Eventive<unit>) =
            let rec loop es =
                Eventive (fun p ->
                    match invokeEventive p (invokeWire es) with
                    | WireNil ->
                        ()
                    | WireCons (a, es') ->
                        invokeEventive p (k a)
                        invokeEventive p (loop es'))
            loop es

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Wire =

    [<CompiledName ("Lift")>]
    let inline lift (x: Wire<'a>) = (^m: (static member From: Wire<'a> -> ^m) (x))
    
    [<CompiledName ("Map")>]
    let rec map f m =
        Wire (Eventive (fun p ->
            let x = invokeEventive p (invokeWire m)
            match x with
            | WireNil ->
                WireNil
            | WireCons (a, m') ->
                WireCons (f a, map f m')))
    
    [<CompiledName ("MapC")>]
    let rec mapc f m =
        Wire (Eventive (fun p ->
            let x = invokeEventive p (invokeWire m)
            match x with
            | WireNil ->
                WireNil
            | WireCons (a, m') ->
                let y = invokeEventive p (f a)
                WireCons (y, mapc f m')))
    
    [<CompiledName ("Ap")>]
    let rec ap mf m =
        Wire (Eventive (fun p ->
            let f = invokeEventive p mf
            let x = invokeEventive p (invokeWire m)
            match x with
            | WireNil ->
                WireNil
            | WireCons (a, m') ->
                WireCons (f a, ap mf m')))
    
    [<CompiledName ("Lift2")>]
    let rec lift2 f m1 m2 =
        Wire (Eventive (fun p ->
            let x1 = invokeEventive p (invokeWire m1)
            let x2 = invokeEventive p (invokeWire m2)
            match (x1, x2) with
            | (WireCons (a1, m1'), 
               WireCons (a2, m2')) ->
                WireCons (f a1 a2, lift2 f m1' m2')
            | _ ->
                WireNil))
    
    [<CompiledName ("Lift3")>]
    let rec lift3 f m1 m2 m3 =
        Wire (Eventive (fun p ->
            let x1 = invokeEventive p (invokeWire m1)
            let x2 = invokeEventive p (invokeWire m2)
            let x3 = invokeEventive p (invokeWire m3)
            match (x1, x2, x3) with
            | (WireCons (a1, m1'), 
               WireCons (a2, m2'),
               WireCons (a3, m3')) ->
                WireCons (f a1 a2 a3, lift3 f m1' m2' m3')
            | _ ->
                WireNil))

    [<CompiledName ("Lift4")>]
    let rec lift4 f m1 m2 m3 m4 =
        Wire (Eventive (fun p ->
            let x1 = invokeEventive p (invokeWire m1)
            let x2 = invokeEventive p (invokeWire m2)
            let x3 = invokeEventive p (invokeWire m3)
            let x4 = invokeEventive p (invokeWire m4)
            match (x1, x2, x3, x4) with
            | (WireCons (a1, m1'), 
               WireCons (a2, m2'),
               WireCons (a3, m3'),
               WireCons (a4, m4')) ->
                WireCons (f a1 a2 a3 a4, lift4 f m1' m2' m3' m4')
            | _ ->
                WireNil))
    
    [<CompiledName ("Lift5")>]
    let rec lift5 f m1 m2 m3 m4 m5 =
        Wire (Eventive (fun p ->
            let x1 = invokeEventive p (invokeWire m1)
            let x2 = invokeEventive p (invokeWire m2)
            let x3 = invokeEventive p (invokeWire m3)
            let x4 = invokeEventive p (invokeWire m4)
            let x5 = invokeEventive p (invokeWire m5)
            match (x1, x2, x3, x4, x5) with
            | (WireCons (a1, m1'), 
               WireCons (a2, m2'),
               WireCons (a3, m3'),
               WireCons (a4, m4'),
               WireCons (a5, m5')) ->
                WireCons (f a1 a2 a3 a4 a5, lift5 f m1' m2' m3' m4' m5')
            | _ ->
                WireNil))

    [<CompiledName ("Zip")>]
    let rec zip m1 m2 =
        Wire (Eventive (fun p ->
            let x1 = invokeEventive p (invokeWire m1)
            let x2 = invokeEventive p (invokeWire m2)
            match (x1, x2) with
            | (WireCons (a1, m1'), 
               WireCons (a2, m2')) ->
                WireCons ((a1, a2), zip m1' m2')
            | _ ->
                WireNil))
    
    [<CompiledName ("Zip3")>]
    let rec zip3 m1 m2 m3 =
        Wire (Eventive (fun p ->
            let x1 = invokeEventive p (invokeWire m1)
            let x2 = invokeEventive p (invokeWire m2)
            let x3 = invokeEventive p (invokeWire m3)
            match (x1, x2, x3) with
            | (WireCons (a1, m1'), 
               WireCons (a2, m2'),
               WireCons (a3, m3')) ->
                WireCons ((a1, a2, a3), zip3 m1' m2' m3')
            | _ ->
                WireNil))
        
    [<CompiledName ("OfEnumerable")>]
    let rec ofSeq (ms: seq<_>) = 
        Wire (Eventive (fun p ->

            let xs = ResizeArray<_> ()
            let ys = ResizeArray<_> ()

            let rec loop (ix: IEnumerator<_>) =
                if ix.MoveNext () then
                    let x = ix.Current
                    match x with
                    | WireNil ->
                        WireNil
                    | WireCons (a, m) ->
                        xs.Add (a)
                        ys.Add (m)
                        loop ix
                elif xs.Count > 0 then
                    WireCons (xs :> seq<_>, ofSeq ys)
                else
                    WireNil

            let loop0 (xs: seq<_>) =
                let ix = xs.GetEnumerator ()
                try
                    loop ix
                finally
                    ix.Dispose ()
                    
            let xs = Seq.map invokeWire ms |> Eventive.ofSeq |> invokeEventive p

            loop0 xs))

    [<CompiledName ("OfList")>]
    let ofList ms = List.toSeq ms |> ofSeq |> map Seq.toList
    
    [<CompiledName ("OfArray")>]
    let ofArray ms = Array.toSeq ms |> ofSeq |> map Seq.toArray

    [<CompiledName ("FromEnumerable")>]
    let fromSeq (es: seq<_>) =
        Wire (eventive.Using (es.GetEnumerator(), fun ie ->
            let rec loop (ie: IEnumerator<_>) = 
                Eventive (fun p ->
                    if ie.MoveNext () then
                        let a = ie.Current
                        WireCons (a, Wire (loop ie))
                    else
                        WireNil)
            loop ie)) 

    [<CompiledName ("Memo")>]
    let rec memo m =
        Wire (Eventive (fun p ->
                let x = invokeEventive p (invokeWire m)
                match x with
                | WireNil ->
                    WireNil
                | WireCons (a, m') ->
                    WireCons (a, memo m')) |> Eventive.memo)

    [<CompiledName ("Unzip")>]
    let unzip m =
        let m = memo m
        (map fst m, map snd m)

    [<CompiledName ("Filter")>]
    let rec filter pred m =
        Wire (Eventive (fun p ->
            let x = invokeEventive p (invokeWire m)
            match x with
            | WireNil ->
                WireNil
            | WireCons (a, m') ->
                if pred a then
                    WireCons (a, filter pred m')
                else
                    invokeEventive p (invokeWire (filter pred m'))))

    [<CompiledName ("FilterC")>]
    let rec filterc pred m =
        Wire (Eventive (fun p ->
            let x = invokeEventive p (invokeWire m)
            match x with
            | WireNil ->
                WireNil
            | WireCons (a, m') ->
                let b = invokeEventive p (pred a)
                if b then
                    WireCons (a, filterc pred m')
                else
                    invokeEventive p (invokeWire (filterc pred m'))))

    [<CompiledName ("Concat")>]
    let concat (mm: Wire<_>) = wire {
        for m in mm do
            yield! m
    }

    [<CompiledName ("Append")>]
    let append m1 m2 = wire.Combine (m1, m2)

    [<CompiledName ("Singleton")>]
    let singleton a = wire.Yield (a)

    [<CompiledName ("SingletonC")>]
    let singletonc m = 
        Wire (Eventive (fun p ->
            let a = invokeEventive p m
            WireCons (a, wire.Zero ())))

    [<CompiledName ("Empty")>]
    let empty<'a> = wire.Zero<'a> ()

    [<CompiledName ("Repeat")>]
    let rec repeat m =
        Wire (Eventive (fun p ->
            let a = invokeEventive p m
            WireCons (a, repeat m)))

    [<CompiledName ("Choice1Of2")>]
    let rec choice1Of2 m =
        Wire (Eventive (fun p ->
            let x = invokeEventive p (invokeWire m)
            match x with
            | WireNil ->
                WireNil
            | WireCons (Choice1Of2 a, m') ->
                WireCons (a, choice1Of2 m')
            | WireCons (Choice2Of2 b, m') ->
                invokeEventive p (invokeWire (choice1Of2 m'))))

    [<CompiledName ("Choice2Of2")>]
    let rec choice2Of2 m =
        Wire (Eventive (fun p ->
            let x = invokeEventive p (invokeWire m)
            match x with
            | WireNil ->
                WireNil
            | WireCons (Choice1Of2 a, m') ->
                invokeEventive p (invokeWire (choice2Of2 m'))
            | WireCons (Choice2Of2 b, m') ->
                WireCons (b, choice2Of2 m')))

    [<CompiledName ("Replace1Of2")>]
    let rec replace1Of2 abs cs =
        Wire (Eventive (fun p ->
            let x = invokeEventive p (invokeWire abs)
            match x with
            | WireNil ->
                WireNil
            | WireCons (Choice1Of2 a, abs') ->
                let y = invokeEventive p (invokeWire cs)
                match y with
                | WireNil ->
                    WireNil
                | WireCons (c, cs') ->
                    WireCons (Choice1Of2 c, replace1Of2 abs' cs')
            | WireCons (Choice2Of2 b, abs') ->
                WireCons (Choice2Of2 b, replace1Of2 abs' cs)))

    [<CompiledName ("Replace2Of2")>]
    let rec replace2Of2 abs cs =
        Wire (Eventive (fun p ->
            let x = invokeEventive p (invokeWire abs)
            match x with
            | WireNil ->
                WireNil
            | WireCons (Choice1Of2 a, abs') ->
                WireCons (Choice1Of2 a, replace2Of2 abs' cs)
            | WireCons (Choice2Of2 b, abs') ->
                let y = invokeEventive p (invokeWire cs)
                match y with
                | WireNil ->
                    WireNil
                | WireCons (c, cs') ->
                    WireCons (Choice2Of2 c, replace2Of2 abs' cs')))

    [<CompiledName ("PartitionChoice")>]
    let partitionChoice abs = 
        let abs = memo abs
        (choice1Of2 abs, choice2Of2 abs)

    [<CompiledName ("ToArrival")>]
    let toArrival m =
        let rec loop xs t0 =
            Wire (Eventive (fun p ->
                let t = p.Time
                let x = invokeEventive p (invokeWire xs)
                match x with
                | WireNil ->
                    WireNil
                | WireCons (a, xs') ->
                    let y =
                        match t0 with
                        | None -> { Value = a; Time = t; Delay = None }
                        | Some t0 -> { Value = a; Time = t; Delay = Some (t - t0) }
                    WireCons (y, loop xs' (Some t))))
        in loop m None

    [<CompiledName ("DelayInTime")>]
    let delayInTime m =
        let rec loop (xs: Wire<_>) t0 a0 =
            Wire (Eventive (fun p ->
                let t = p.Time;
                if t = t0 then
                    WireCons (a0, loop xs t0 a0)
                elif t > t0 then
                    let x = invokeEventive p (invokeWire xs)
                    match x with
                    | WireNil ->
                        WireNil
                    | WireCons (a, xs') ->
                        WireCons (a, loop xs' t a)
                else
                    failwithf "The time is not synchronized with the event queue."))
        in Wire (Eventive (fun p ->
                let t = p.Time
                let x = invokeEventive p (invokeWire m)
                match x with
                | WireNil ->
                    WireNil
                | WireCons (a, xs') ->
                    WireCons (a, loop xs' t a)))
