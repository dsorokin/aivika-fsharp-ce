
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

[<Sealed>]
type Ref<'a> private (x0: 'a, changedSource: SignalSource<'a>) =

    let mutable x = x0

    let read = 
        Eventive (fun p -> x)

    let write a = 
        Eventive (fun p ->
            x <- a
            SignalSource.trigger a changedSource
                |> invokeEventive p)

    let modify f =
        Eventive (fun p ->
            let a = x
            let b = f a
            x <- b
            SignalSource.trigger b changedSource
                |> invokeEventive p)

    let changed  = SignalSource.publish changedSource
    let changed_ = changed |> Signal.map (fun a -> ())

    member internal x.Read () = read
    member internal x.Write (a) = write a
    member internal x.Modify (f) = modify f

    member internal x.Changed = changed
    member internal x.Changed_ = changed_

    interface ISignalable<'a> with

        member x.Read () = read
        member x.Changed_ = changed_

    static member Create (a: 'a) = 
        Simulation (fun r ->
            let s = SignalSource.create |> invokeSimulation r
            Ref (a, s))

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Ref =

    let private incf = fun x -> x + 1
    let private decf = fun x -> x - 1

    [<CompiledName ("Create")>]
    let create a = Ref<_>.Create (a)

    [<CompiledName ("Read")>]
    let read (r: Ref<_>) = r.Read ()

    [<CompiledName ("Write")>]
    let write a (r: Ref<_>) = r.Write (a)

    [<CompiledName ("Modify")>]
    let modify f (r: Ref<_>) = r.Modify (f)

    [<CompiledName ("Inc")>]
    let inc (r: Ref<_>) = r.Modify (incf)

    [<CompiledName ("Dec")>]
    let dec (r: Ref<_>) = r.Modify (decf)

    [<CompiledName ("Changed")>]
    let changed (r: Ref<_>) = r.Changed

    [<CompiledName ("Changed_")>]
    let changed_ (r: Ref<_>) = r.Changed_
