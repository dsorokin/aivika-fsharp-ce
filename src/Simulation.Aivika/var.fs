
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

[<Sealed>]
type Var<'a> private (x0: Time, y0: 'a, changedSource: SignalSource<'a>) =

    let xs = List<_>()
    let ms = List<_>()
    let ys = List<_>()
    
    do xs.Add (x0)
    do ms.Add (y0)
    do ys.Add (y0)

    let read = 
        Eventive (fun p ->
            let t = p.Time
            let count = xs.Count
            let i = count - 1
            let x = xs.[i]
            if x <= t then
                ys.[i]
            else
                let i = xs.BinarySearch (t)
                if i >= 0 then
                    ys.[i]
                else
                    ys.[(~~~i) - 1])

    let memo = 
        Eventive (fun p ->
            let t = p.Time
            let count = xs.Count
            let i = count - 1
            let x = xs.[i]
            if x < t then
                let a = ys.[i]
                xs.Add (t)
                ms.Add (a)
                ys.Add (a)
                a
            elif x = t then
                ms.[i]
            else
                let i = xs.BinarySearch (t)
                if i >= 0 then
                    ms.[i]
                else
                    ms.[(~~~i) - 1])
            |> Eventive.runWith CurrentEventsOrFromPast

    let write a = 
        Eventive (fun p ->
            let t = p.Time
            let count = xs.Count
            let i = count - 1
            let x = xs.[i]
            if t < x then
                failwithf "Cannot update the past data."
            elif t = x then
                ys.[i] <- a
            else
                xs.Add (t)
                ms.Add (a)
                ys.Add (a)
            SignalSource.trigger a changedSource
                |> invokeEventive p)

    let modify f =
        Eventive (fun p ->
            let t = p.Time
            let count = xs.Count
            let i = count - 1
            let x = xs.[i]
            if t < x then
                failwithf "Cannot update the past data."
            elif t = x then
                let a = ys.[i]
                let b = f a
                ys.[i] <- b
                SignalSource.trigger b changedSource
                    |> invokeEventive p
            else
                let a = ys.[i]
                let b = f a
                xs.Add (t)
                ms.Add (b)
                ys.Add (b)
                SignalSource.trigger b changedSource
                    |> invokeEventive p)

    let freeze =
        Eventive (fun p ->
            (xs.ToArray (), ms.ToArray (), ys.ToArray ()))

    let changed  = SignalSource.publish changedSource
    let changed_ = changed |> Signal.map (fun a -> ())

    member internal x.Read () = read
    member internal x.Memo () = memo
    member internal x.Write (a) = write a
    member internal x.Modify (f) = modify f
    member internal x.Freeze () = freeze

    member internal x.Changed = changed
    member internal x.Changed_ = changed_

    interface ISignalable<'a> with

        member x.Read () = read
        member x.Changed_ = changed_

    static member Create (a: 'a) = 
        Simulation (fun r ->
            let s = SignalSource.create |> invokeSimulation r
            Var (r.Specs.StartTime, a, s))

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Var =

    let private incf = fun x -> x + 1
    let private decf = fun x -> x - 1

    [<CompiledName ("Create")>]
    let create a = Var<_>.Create (a)

    [<CompiledName ("Read")>]
    let read (v: Var<_>) = v.Read ()

    [<CompiledName ("Memo")>]
    let memo (v: Var<_>) = v.Memo ()

    [<CompiledName ("Write")>]
    let write a (v: Var<_>) = v.Write (a)

    [<CompiledName ("Modify")>]
    let modify f (v: Var<_>) = v.Modify (f)

    [<CompiledName ("Inc")>]
    let inc (v: Var<_>) = v.Modify (incf)

    [<CompiledName ("Dec")>]
    let dec (v: Var<_>) = v.Modify (decf)

    [<CompiledName ("Freeze")>]
    let freeze (v: Var<_>) = v.Freeze ()

    [<CompiledName ("Changed")>]
    let changed (v: Var<_>) = v.Changed

    [<CompiledName ("Changed_")>]
    let changed_ (v: Var<_>) = v.Changed_
