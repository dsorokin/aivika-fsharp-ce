
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

open Simulation.Aivika.Collections

type Time = float

type Iteration = int

type Phase = int

type Method = Euler | RungeKutta2 | RungeKutta4

type Specs = 
    { StartTime: float; 
      StopTime: float; 
      DT: float; 
      Method: Method;
      GeneratorType: GeneratorType }

and internal Run =
    { Specs: Specs;
      Index: int;
      Count: int;
      EventQueue: EventQueue;
      UniqueId: int;
      Generator: Generator }

and internal Point =
    { Run: Run;
      Specs: Specs; 
      Time: Time;
      Iteration: Iteration; 
      Phase: Phase }

and [<Sealed>] internal EventQueue (t0: Time) =
    
    let pq = new PriorityQueue<_> ()

    let mutable busy = false

    let mutable time = t0

    let rec loop (p: Point) includingCurrentEvents =

        if not pq.IsEmpty then
            let t2 = pq.FrontKey
            if (t2 < time) then
                failwithf "The time value is too small. The event queue is desynchronized."
            if (t2 < p.Time) || (includingCurrentEvents && (t2 = p.Time)) then
                time <- t2
                let c2 = pq.FrontValue
                pq.Dequeue ()
                let run = p.Run
                let sc = run.Specs
                let t0 = sc.StartTime
                let dt = sc.DT
                let n2 = int (floor ((t2 - t0) / dt))
                c2 { Run = run;
                     Specs = sc;
                     Time = t2;
                     Iteration = n2;
                     Phase = -1 }
                loop p includingCurrentEvents

    member x.Count =
        pq.Count

    member x.Enqueue (t: Time, h: Point -> unit) =
        pq.Enqueue (t, h)

    member x.Run (p, includingCurrentEvents) =

        if not busy then

            busy <- true
            loop p includingCurrentEvents
            busy <- false

    member x.RunSync (p, includingCurrentEvents) =
    
        if p.Time < time
            then failwithf "The current time is less than the actual time of the queue."
            else x.Run (p, includingCurrentEvents) 

    member x.Point (run: Run) =

        let sc = run.Specs
        let t0 = sc.StartTime
        let dt = sc.DT
        let n2 = int (floor ((time - t0) / dt))

        { Run = run;
          Specs = sc;
          Time = time;
          Iteration = n2;
          Phase = -1 }

type internal UniqueIdGenerator private () =

    static let idlock = obj ()
    static let mutable id = 0

    static member Next () =

        lock idlock (fun () -> 
            id <- 1 + id
            id)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Specs =

    let iterationCount specs =
        1 + int (ceil ((specs.StopTime - specs.StartTime) / specs.DT)) : Iteration
        
    let phaseCount specs = 
        match specs.Method with
            | Euler -> 1 : Phase
            | RungeKutta2 -> 2 : Phase
            | RungeKutta4 -> 4 : Phase

    let integTime specs n ph =
        let delta m ph = 
            match m, ph with
                | Euler, 0 -> 0.0
                | RungeKutta2, 0 -> 0.0
                | RungeKutta2, 1 -> specs.DT
                | RungeKutta4, 0 -> 0.0
                | RungeKutta4, 1 -> specs.DT / 2.0
                | RungeKutta4, 2 -> specs.DT / 2.0
                | RungeKutta4, 3 -> specs.DT
                | _  -> failwith "Incorrect method and/or phase"
        in specs.StartTime + float (n) * specs.DT + delta specs.Method ph

    let integTimes specs = seq {

        for n = 0 to (iterationCount specs - 1) do
            yield integTime specs n 0 
    }

type Specs with

    member x.IterationCount = Specs.iterationCount x
    member x.PhaseCount = Specs.phaseCount x
    member x.IntegTimes = Specs.integTimes x

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module internal Run =

    let integPoints (run: Run) = seq {

        let sc = run.Specs

        for n = 0 to (Specs.iterationCount sc - 1) do
            yield { Run = run;
                    Specs = sc;
                    Time = Specs.integTime sc n 0;
                    Iteration = n;
                    Phase = 0 } 
    }

    let integStartPoint (run: Run) =

        let sc = run.Specs

        { Run = run;
          Specs = sc;
          Time = sc.StartTime;
          Iteration = 0;
          Phase = 0 }

    let integStopPoint (run: Run) =

        let sc = run.Specs

        { Run = run;
          Specs = sc;
          Time = sc.StopTime;
          Iteration = Specs.iterationCount sc - 1;
          Phase = 0 }

    let point (run: Run) t =

        let sc = run.Specs
        let t0 = sc.StartTime
        let dt = sc.DT
        let n  = int (floor ((t - t0) / dt))
    
        { Run = run;
          Specs = sc;
          Time = t;
          Iteration = n;
          Phase = -1 }

type Run with

    member x.IntegPoints = Run.integPoints x
    member x.IntegStartPoint = Run.integStartPoint x
    member x.IntegStopPoint = Run.integStopPoint x

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module internal Point =

    let integPointsFrom (p: Point) = seq {

        let run = p.Run
        let sc = p.Specs
        let n0 = if p.Phase = 0 then p.Iteration else p.Iteration + 1

        for n = n0 to (Specs.iterationCount sc - 1) do
            yield { Run = run;
                    Specs = sc;
                    Time = Specs.integTime sc n 0;
                    Iteration = n;
                    Phase = 0 } 
    }

type Point with

    member x.IntegPointsFromThis = Point.integPointsFrom x
