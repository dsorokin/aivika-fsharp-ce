
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

/// Represents a time dependent computation synchronized with the event queue.
[<Sealed; NoEquality; NoComparison>]
type Eventive<'a> =

    /// Creates a new computation.
    internal new: f:(Point -> 'a) -> Eventive<'a>

    /// Returns the underlying function.
    member internal Fun: (Point -> 'a)

    /// Lifts a computation.
    static member From: comp:Parameter<'a> -> Eventive<'a>

    /// Lifts a computation.
    static member From: comp:Simulation<'a> -> Eventive<'a>

    /// Lifts a computation.
    static member From: comp:Dynamics<'a> -> Eventive<'a>

    /// Lifts a computation.
    static member From: comp:Eventive<'a> -> Eventive<'a>

/// Contains a function that allows invoking the Eventive computation.
[<AutoOpen>]
module internal EventiveInvoke =

    /// Invokes the computation with the specified time point.
    val inline invokeEventive: p:Point -> comp:Eventive<'a> -> 'a

/// Represents a builder for constructing Eventive computations.
[<Sealed>]
type EventiveBuilder =

    /// Creates a computation that returns the specified value.
    member Return: value:'a -> Eventive<'a>
    
    /// Delegates to the input computation.
    member ReturnFrom: comp:Eventive<'a> -> Eventive<'a>
    
    /// Creates a computation that binds the specified computation with its continuation.
    member Bind: comp:Eventive<'a> * cont:('a -> Eventive<'b>) -> Eventive<'b>
    
    /// Creates a computation that runs the specified one.
    member Delay: comp:(unit -> Eventive<'b>) -> Eventive<'b>
    
    /// Creates a computation that just returns ().
    member Zero: unit -> Eventive<unit>
    
    /// Creates a computation that runs the first computation and then runs the second one, returning the result of the latter.
    member Combine: comp1:Eventive<unit> * comp2:Eventive<'a> -> Eventive<'a>
    
    /// Creates a computation that enumerates the sequence on demand and runs body for each element.
    member For: comp:seq<'a> * body:('a -> Eventive<unit>) -> Eventive<unit>
    
    /// Creates a computation that runs the specified computation repeatedly until the provided guard becomes false.
    member While: pred:(unit -> bool) * body:Eventive<unit> -> Eventive<unit>
    
    /// Creates a computation that allows using the disposable resource.
    member Using: resource:'a * body:('a -> Eventive<'b>) -> Eventive<'b> when 'a :> IDisposable
    
    /// Creates a computation with the finalization part.
    member TryFinally: comp:Eventive<'a> * finalizer:(unit -> unit) -> Eventive<'a>
    
    /// Creates a computation with the exception handling.
    member TryWith: comp:Eventive<'a> * handler:(exn -> Eventive<'a>) -> Eventive<'a>

/// Contains a builder of the Eventive computation.
[<AutoOpen>]
module EventiveWorkflow =     

    /// The builder of the Eventive computation.
    val eventive: EventiveBuilder

/// Defines how the events are processed.
type EventProcessing =
    /// Either process all earlier and then current events, or raise an error if the current simulation time is less than the actual time of the event queue.
    | CurrentEvents
    /// Either process all earlier events not affecting the events at the current simulation time, or raise an error if the current simulation time is less than the actual time of the event queue.
    | EarlierEvents
    /// Either process all earlier and then current events, or do nothing if the current simulation time is less than the actual time of the event queue (do not use unless the documentation states the opposite).
    | CurrentEventsOrFromPast
    /// Either process all earlier events, or do nothing if the current simulation time is less than the actual time of the event queue (do not use unless the documentation states the opposite).
    | EarlierEventsOrFromPast

/// It allows cancelling the event.
type EventCancellation =

    /// Tests whether the event was cancelled.
    abstract Cancelled: Eventive<bool>

    /// Tests whether the event was successfully processed.
    abstract Finished: Eventive<bool>

    /// Cancels the event as possible.
    abstract Cancel: unit -> Eventive<unit>

/// The module contains useful functions for working with the Eventive computations.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Eventive =

    /// Runs a computation specifying how other pending event handlers should be involved in the simulation.
    [<CompiledName ("RunWith")>]
    val runWith: processing:EventProcessing -> comp:Eventive<'a> -> Dynamics<'a>

    /// Runs a computation involving all current pending events in the simulation.
    [<CompiledName ("Run")>]
    val run: comp:Eventive<'a> -> Dynamics<'a>
    
    /// Runs a computation in the start time point.
    [<CompiledName ("RunInStartTime")>]
    val runInStartTime: comp:Eventive<'a> -> Simulation<'a>
    
    /// Runs a computation in the final time point.
    [<CompiledName ("RunInStopTime")>]
    val runInStopTime: comp:Eventive<'a> -> Simulation<'a>

    /// Lifts a computation.
    [<CompiledName ("Lift")>]
    val inline lift: comp:Eventive<'a> -> ^m when ^m: (static member From: Eventive<'a> -> ^m)

    /// Computation that returns the number of pending events that should be yet actuated.
    [<CompiledName ("QueueCount")>]
    val queueCount: Eventive<int>

    /// Enqueues the event which handler must be actuated at the specified time.
    [<CompiledName ("Enqueue")>]
    val enqueue: time:Time -> handler:Eventive<unit> -> Eventive<unit>

    /// Enqueues the event with an ability to cancel it.
    [<CompiledName ("EnqueueWithCancellation")>]
    val enqueueWithCancellation: time:Time -> handler:Eventive<unit> -> Eventive<EventCancellation>

    /// Actuates the event handler in the specified times.
    [<CompiledName ("EnqueueWithTimes")>]
    val enqueueWithTimes: times:#seq<Time> -> handler:Eventive<unit> -> Eventive<unit>
    
    /// Actuates the event handler in the specified time points.
    [<CompiledName ("EnqueueWithPoints")>]
    val internal enqueueWithPoints: points:#seq<Point> -> handler:Eventive<unit> -> Eventive<unit>

    /// Actuates the event handler in the integration time points.
    [<CompiledName ("EnqueueWithIntegTimes")>]
    val enqueueWithIntegTimes: handler:Eventive<unit> -> Eventive<unit>

    /// Flattens the nested computation.
    [<CompiledName ("Concat")>]
    val concat: comp:Eventive<Eventive<'a>> -> Eventive<'a>
    
    /// Maps a function over a computation.
    [<CompiledName ("Map")>]
    val map: f:('a -> 'b) -> comp:Eventive<'a> -> Eventive<'b>
    
    /// Transforms a computation.
    [<CompiledName ("Ap")>]
    val ap: f:Eventive<'a -> 'b> -> comp:Eventive<'a> -> Eventive<'b>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift2")>]
    val lift2: f:('a -> 'b -> 'c) -> comp1:Eventive<'a> -> comp2:Eventive<'b> -> Eventive<'c>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift3")>]
    val lift3: f:('a -> 'b -> 'c -> 'd) -> comp1:Eventive<'a> -> comp2:Eventive<'b> -> comp3:Eventive<'c> -> Eventive<'d>

    /// Promotes a function to computations.
    [<CompiledName ("Lift4")>]
    val lift4: f:('a -> 'b -> 'c -> 'd -> 'e) -> comp1:Eventive<'a> -> comp2:Eventive<'b> -> comp3:Eventive<'c> -> comp4:Eventive<'d> -> Eventive<'e>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift5")>]
    val lift5: f:('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> comp1:Eventive<'a> -> comp2:Eventive<'b> -> comp3:Eventive<'c> -> comp4:Eventive<'d> -> comp5:Eventive<'e> -> Eventive<'f>

    /// Takes two computations and creates a new computation of the corresponding pairs.  
    [<CompiledName ("Zip")>]
    val zip: comp1:Eventive<'a> -> comp2:Eventive<'b> -> Eventive<'a * 'b>
    
    /// Takes three computations and creates a new computation of the corresponding triples.
    [<CompiledName ("Zip3")>]
    val zip3: comp1:Eventive<'a> -> comp2:Eventive<'b> -> comp3:Eventive<'c> -> Eventive<'a * 'b * 'c>
    
    /// Takes a list of computations and creates a new computation of the corresponding lists.
    [<CompiledName ("OfList")>]
    val ofList: comps:Eventive<'b> list -> Eventive<'b list>
    
    /// Takes an array of computations and creates a new computation of the corresponding arrays.
    [<CompiledName ("OfArray")>]
    val ofArray: comps:Eventive<'b> [] -> Eventive<'b []>
        
    /// Takes a sequence of computations and creates a new computation of the corresponding sequences.
    [<CompiledName ("OfEnumerable")>]
    val ofSeq: comps:#seq<Eventive<'b>> -> Eventive<seq<'b>>

    /// Converts the action to an IDisposable object within the current computation.
    [<CompiledName ("ToDisposable")>]
    val toDisposable: comp:Eventive<unit> -> Eventive<IDisposable>

    /// Computes the value lazily within the current computation.
    [<CompiledName ("ToLazy")>]
    val toLazy: comp:Eventive<'a> -> Eventive<Lazy<'a>>
   
    /// Memoizes the computation so that a new computation would always return the same result within the simulation run.
    [<CompiledName ("Memo")>]
    val memo: comp:Eventive<'a> -> Eventive<'a>

    /// Shows the debug message with the current simulation time.
    [<CompiledName ("Trace")>]
    val trace: message:string -> comp:Eventive<'a> -> Eventive<'a>
