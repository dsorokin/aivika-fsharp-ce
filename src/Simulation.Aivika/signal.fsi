
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

/// Represents a signal that can have disposable handlers.
[<AbstractClass; NoEquality; NoComparison>]
type Signal<'a> =

    /// Creates a new signal.
    new: unit -> Signal<'a>

    /// Subscribes the handler to the specified signal within a computation and returns a disposable object that, being invoked, unsubscribes the handler from this signal.
    abstract Subscribe: handler:('a -> Eventive<unit>) -> Eventive<IDisposable> 

/// Add extensions to the Signal class type.
[<AutoOpen>]
module SignalExtensions =

    type Signal<'a> with

        /// Permanently connects the handler to the signal.
        member Add: handler:('a -> Eventive<unit>) -> Eventive<unit>

/// Represents a signal soruce that can publish and tigger its signal.
[<Sealed>]
type SignalSource<'a>

/// The module contains useful functions for working with the signal.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Signal =

    /// Permanently connects the handler to the signal.
    [<CompiledName ("Add")>]
    val add: handler:('a -> Eventive<unit>) -> comp:Signal<'a> -> Eventive<unit>

    /// Subscribes the handler to the specified signal within a computation and returns a disposable object that, being invoked, unsubscribes the handler from this signal.
    [<CompiledName ("Subscribe")>]
    val subscribe: handler:('a -> Eventive<unit>) -> comp:Signal<'a> -> Eventive<IDisposable>

    /// Maps a function over a signal.
    [<CompiledName ("Map")>]
    val map: f:('a -> 'b) -> comp:Signal<'a> -> Signal<'b>

    /// Composes the signal.
    [<CompiledName ("MapC")>]
    val mapc: f:('a -> Eventive<'b>) -> comp:Signal<'a> -> Signal<'b>

    /// Transforms the signal.
    [<CompiledName ("Ap")>]
    val ap: f:Eventive<'a -> 'b> -> comp:Signal<'a> -> Signal<'b>

    /// Filters only those signal values that satisfy the given predicate.
    [<CompiledName ("Filter")>]
    val filter: pred:('a -> bool) -> comp:Signal<'a> -> Signal<'a>

    /// Filters only those signal values that satisfy the given predicate.
    [<CompiledName ("Filter_")>]
    val filter_: pred:('a -> bool) -> comp:Signal<'a> -> Signal<unit>

    /// Filters only those signal values that satisfy the given predicate within the computation.
    [<CompiledName ("FilterC")>]
    val filterc: pred:('a -> Eventive<bool>) -> comp:Signal<'a> -> Signal<'a>

    /// Filters only those signal values that satisfy the given predicate within the computation.
    [<CompiledName ("FilterC_")>]
    val filterc_: pred:('a -> Eventive<bool>) -> comp:Signal<'a> -> Signal<unit>

    /// An empty signal which is never triggered.
    [<CompiledName ("Empty")>]
    val empty<'a> : Signal<'a>

    /// Merges two signals.
    [<CompiledName ("Merge")>]
    val merge: comp1:Signal<'a> -> comp2:Signal<'a> -> Signal<'a>

    /// Concatenates the signals.
    [<CompiledName ("Concat")>]
    val concat: comps:#Signal<'a> list -> Signal<'a>

    /// Returns a signal which is triggered in the specified time points.
    [<CompiledName ("InTimes")>]
    val inTimes: times:#seq<Time> -> Eventive<Signal<Time>>

    /// Returns a signal which is triggered in the integration time points.
    [<CompiledName ("InIntegTimes")>]
    val inIntegTimes: Eventive<Signal<Time>>

    /// Returns a signal which is triggered in the start time point.
    [<CompiledName ("InStartTime")>]
    val inStartTime: Eventive<Signal<Time>>

    /// Returns a signal which is triggered in the final time point.
    [<CompiledName ("InStopTime")>]
    val inStopTime: Eventive<Signal<Time>>

    /// Transforms a signal so that the resulting signal returns a sequence of arrivals saving the information about the time points at which the original signal was received.
    [<CompiledName ("ToArrival")>]
    val toArrival: comp:Signal<'a> -> Signal<Arrival<'a>>

    /// Delays the signal values for the specified time interval.
    [<CompiledName ("Delay")>]
    val delay: interval:float -> comp:Signal<'a> -> Signal<'a>

    /// Delays the signal values for time intervals recalculated for each value.
    [<CompiledName ("DelayC")>]
    val delayc: interval:Eventive<float> -> comp:Signal<'a> -> Signal<'a>

/// The module contains useful functions for working with the signal source.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module SignalSource =

    /// Creates a new signal source within the computation.
    [<CompiledName ("Create")>]
    val create<'a> : Simulation<SignalSource<'a>>

    /// Publishes the signal.
    [<CompiledName ("Publish")>]
    val publish: source:SignalSource<'a> -> Signal<'a>

    /// Triggers the signal actuating all its subscribed handlers at the current simulation time point.
    [<CompiledName ("Trigger")>]
    val trigger: value:'a -> source:SignalSource<'a> -> Eventive<unit>

/// Contains the history of the signal values.
[<Sealed>]
type SignalHistory<'a>

/// The module contains useful functions for working with the signal history.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module SignalHistory =

    /// Creates a history of the signal values.
    [<CompiledName ("Create")>]
    val create: signal:Signal<'a> -> Eventive<SignalHistory<'a>>

    /// Reads the history of the signal values.
    [<CompiledName ("Read")>]
    val read: history:SignalHistory<'a> -> Eventive<Time array * 'a array>

/// Represents a computation that also signals when changing its value.
[<NoEquality; NoComparison>]
type ISignalable<'a> =

    /// Returns a computation of the value.
    abstract Read: unit -> Eventive<'a>

    /// Returns a signal that notifies about changing the computed value.
    abstract Changed_: Signal<unit>

/// An implementation of the computation that signals when changing its value.
[<NoEquality; NoComparison>]
type Signalable<'a> =

    /// Initializes a new instance.
    new: comp:Eventive<'a> * signal:Signal<unit> -> Signalable<'a>

    interface ISignalable<'a>

/// Adds extensions to the ISignalable interface.
[<AutoOpen>]
module SignalableExtensions =

    type ISignalable<'a> with

        /// Returns a signal that notifies about changing the computed value.
        member Changed: Signal<'a>

/// The module contains useful functions for working with a computation that signals about changing its state.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Signalable =

    /// Returns a computation of the value.
    [<CompiledName ("Read")>]
    val read: signalable:ISignalable<'a> -> Eventive<'a>

    /// Returns a signal that notifies about changing the computed value.
    [<CompiledName ("Changed_")>]
    val changed_: signalable:ISignalable<'a> -> Signal<unit>

    /// Returns a signal that notifies about changing the computed value.
    [<CompiledName ("Changed")>]
    val changed: signalable:ISignalable<'a> -> Signal<'a>

    /// Maps a function over a computation.
    [<CompiledName ("Map")>]
    val map: f:('a -> 'b) -> signalable:ISignalable<'a> -> ISignalable<'b>

/// It representes the predefined signals provided by every simulation model.
type PredefinedSignalSet =
    { /// The signal triggered in the integration time points.
      SignalInIntegTimes: Signal<Time>;
      /// The signal triggered in the start time.
      SignalInStartTime: Signal<Time>;
      /// The signal triggered in the stop time.
      SignalInStopTime: Signal<Time> }

/// This module contains functions for working with the predefined signals.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module PredefinedSignalSet =

    /// Creates a set of predefined signals in the current (must be initial) time point.
    [<CompiledName ("Create")>]
    val create: Eventive<PredefinedSignalSet>
