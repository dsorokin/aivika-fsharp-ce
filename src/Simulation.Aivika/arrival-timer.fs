
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

type ArrivalTimer =
    { mutable ProcessingTime: SamplingStats<float>;
      mutable ProcessingTimeChangedSource: SignalSource<unit> }

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ArrivalTimer =

    [<CompiledName ("Create")>]
    let create = simulation {
            let! s = SignalSource.create
            return { ProcessingTime = SamplingStats.emptyFloats;
                     ProcessingTimeChangedSource = s }
        }

    [<CompiledName ("ProcessingTime")>]
    let processingTime (timer: ArrivalTimer) =
        Eventive (fun p -> timer.ProcessingTime)

    [<CompiledName ("ProcessingTimeChanged_")>]
    let processingTimeChanged_ (timer: ArrivalTimer) =
        timer.ProcessingTimeChangedSource |> SignalSource.publish

    [<CompiledName ("ProcessingTimeChanged")>]
    let processingTimeChanged (timer: ArrivalTimer) =
        processingTimeChanged_ timer |> Signal.map (fun () -> timer.ProcessingTime)

    [<CompiledName ("Processor")>]
    let processor (timer:ArrivalTimer) = 
        let rec loop m = Stream (proc {
                let! x = invokeStream m
                match x with
                | StreamNil ->
                    return StreamNil
                | StreamCons (a: Arrival<_>, m') ->
                    do! eventive {
                            let! t = Dynamics.time |> Dynamics.lift
                            timer.ProcessingTime <- 
                                timer.ProcessingTime
                                    |> SamplingStats.add (t - a.Time)
                        } |> Eventive.lift
                    return StreamCons (a, loop m')
            })
        in loop
