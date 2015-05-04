
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

/// Specifies the simulation time.
type Time = float

/// Specifies the iteration number when integrating.
type Iteration = int

/// Specifies the phase when integrating.
type Phase = int

/// Specifies the integration method.
type Method = Euler | RungeKutta2 | RungeKutta4

/// Represents the simulation specs.
type Specs = 
    { /// Specifies the start simulation time.
      StartTime: Time; 
      /// Specifies the final simulation time.
      StopTime: Time; 
      /// Specifies the integration time step.
      DT: Time; 
      /// Specifies the integration method.
      Method: Method;
      /// Specifies the type of the pseudo-random number generator.
      GeneratorType: GeneratorType }

/// Represents the simulation run.
and internal Run =
    { /// Defines the simulation specs.
      Specs: Specs;
      /// Specifies the run index starting from zero.
      Index: int;
      /// Specifies the total number of runs.
      Count: int;
      /// Returns the event queue.
      EventQueue: EventQueue;
      /// Returns the unique identifier assigned to this simulation run.
      UniqueId: int;
      /// Returns the random number generator for this run.
      Generator: Generator }

/// Represents the simulation time point.
and internal Point =
    { /// Specifies the simulation run.
      Run: Run;
      /// Defines the simulation specs.
      Specs: Specs; 
      /// Specifies the current time.
      Time: Time;
      /// Specifies the closest integration iteration.
      Iteration: Iteration; 
      /// Specifies the integration phase.
      Phase: Phase }

/// Represents the event queue.
and [<Sealed>] internal EventQueue =

    /// Creates a new event queue with the specified initial actual time.
    new: t0:Time -> EventQueue
    
    /// Returns the number of pending events to be actuated.
    member Count: int

    /// Enqueues the event handler at the specified time.
    member Enqueue: t:Time * f:(Point -> unit) -> unit

    /// Actuates the pending events including the current ones if required.
    member Run: p:Point * includingCurrentEvents:bool -> unit

    /// Actuates synchronously the pending events inluding the current ones if required.
    member RunSync: p:Point * includingCurrentEvents:bool -> unit

    /// Returns the actual time point.
    member Point: r:Run -> Point

/// Represents a generator of unique identifiers.
[<Class>]
type internal UniqueIdGenerator =

    /// Generates the next unique identifier.
    static member Next: unit -> int

/// This module contains helper functions for working with the simulation specs.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Specs =

    /// Returns the total number of the integration iterations.
    val iterationCount: specs:Specs -> Iteration

    /// Returns the maximum number of phases that can be used when integrating.
    val phaseCount: specs:Specs -> Phase

    /// Returns the integration time by the specified specs, iteration and phase.
    val integTime: specs:Specs -> n:Iteration -> ph:Phase -> Time

    /// Returns the integration times.
    val integTimes: specs:Specs -> seq<Time>

type Specs with

    /// Returns the total number of integration interations.
    member IterationCount: Iteration

    /// Returns the maximum number of phases used when integrating.
    member PhaseCount: Phase
    
    /// Returns the integration times.
    member IntegTimes: seq<Time>

/// This module contains helper functions for working with the simulation run.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module internal Run =

    /// Returns the integration time points.
    val integPoints: r:Run -> seq<Point>

    /// Returns the start integration time point.
    val integStartPoint: r:Run -> Point

    /// Returns the final integration time point.
    val integStopPoint: r:Run -> Point

    /// Returns the point at the specified time.
    val point: r:Run -> t:Time -> Point

type Run with

    /// Returns the integration time points.
    member IntegPoints: seq<Point>

    /// Returns the start integration time point.
    member IntegStartPoint: Point
    
    /// Returns the final integration time point.
    member IntegStopPoint: Point

/// This module contains helper functions for working with the simulation point.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module internal Point =

    /// Returns the integration time points started from the specified one.
    val integPointsFrom: p:Point -> seq<Point>

type Point with

    /// Returns the integration time points started from the specified one.
    member IntegPointsFromThis: seq<Point>
