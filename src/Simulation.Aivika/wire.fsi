
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

/// Represents a wire item at the current time point.
type internal WireItem<'a> =
    | WireNil
    | WireCons of 'a * Wire<'a>

/// Represents a wire that supplies values at the current simulation time point by demand.
and [<Sealed; NoEquality; NoComparison>] Wire<'a> =

    /// Creates a new computation.
    internal new: comp:Eventive<WireItem<'a>> -> Wire<'a>

    /// Returns the underlying computation.
    member internal Eventive: Eventive<WireItem<'a>>

    /// Lifts a computation.
    static member From: comp:Parameter<unit> -> Wire<'a>

    /// Lifts a computation.
    static member From: comp:Simulation<unit> -> Wire<'a>

    /// Lifts a computation.
    static member From: comp:Dynamics<unit> -> Wire<'a>

    /// Lifts a computation.
    static member From: comp:Eventive<unit> -> Wire<'a>

    /// Lifts a computation.
    static member From: comp:Wire<'a> -> Wire<'a>

/// Contains a function that allows invoking the Eventable computation.
[<AutoOpen>]
module internal WireInvoke =

    /// Invokes the computation.
    val inline invokeWire: comp:Wire<'a> -> Eventive<WireItem<'a>>

/// Represents a builder for constructing Wire computations.
[<Sealed>]
type WireBuilder =

    /// Creates a computation that yields the specified value.
    member Yield: value:'a -> Wire<'a>
    
    /// Delegates to the input computation.
    member YieldFrom: comp:Wire<'a> -> Wire<'a>

    /// Creates a computation that yields nothing.
    member Return: value:'u -> Wire<'a>
    
    /// Creates a computation that performs side effect but yields nothing.
    member ReturnFrom: comp:Eventive<unit> -> Wire<'a>
    
    /// Creates a computation that binds the specified computation with its continuation.
    member Bind: comp:Eventive<'a> * cont:('a -> Wire<'b>) -> Wire<'b>
    
    /// Creates a computation that runs the specified one.
    member Delay: comp:(unit -> Wire<'b>) -> Wire<'b>
    
    /// Creates a computation that does nothing.
    member Zero<'a> : unit -> Wire<'a>
    
    /// Creates a computation that combines the both input computations.
    member Combine: comp1:Wire<'a> * comp2:Wire<'a> -> Wire<'a>

    /// Creates a computation that enumerates the stream on demand and runs body for each element.
    member For: comp:Wire<'a> * body:('a -> Wire<'b>) -> Wire<'b>

    /// Creates a computation that enumerates the sequence on demand and runs body for each element.
    member For: comp:seq<'a> * body:('a -> Wire<'b>) -> Wire<'b>
    
    /// Creates a computation that runs the specified computation repeatedly until the provided guard becomes false.
    member While: pred:(unit -> bool) * body:Wire<'b> -> Wire<'b>
    
    /// Creates a computation that allows using the disposable resource.
    member Using: resource:'a * body:('a -> Wire<'b>) -> Wire<'b> when 'a :> IDisposable
    
    /// Creates a computation with the finalization part.
    member TryFinally: comp:Wire<'a> * finalizer:(unit -> unit) -> Wire<'a>
    
    /// Creates a computation with the exception handling.
    member TryWith: comp:Wire<'a> * handler:(exn -> Wire<'a>) -> Wire<'a>

/// Contains a builder of the Wire computation.
[<AutoOpen>]
module WireWorkflow =     

    /// The builder of the Wire computation.
    val wire: WireBuilder

/// This module defines some extensions for the Eventable computation builder.
[<AutoOpen>]
module EventiveBuilderExtensions =

    type EventiveBuilder with

        /// Creates a computation that enumerates the wire on demand and runs body for each element.
        member For: comp:Wire<'a> * body:('a -> Eventive<unit>) -> Eventive<unit>
    
/// The module contains useful functions for working with the Wire computation.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Wire =

    /// Lifts a computation.
    [<CompiledName ("Lift")>]
    val inline lift: comp:Wire<'a> -> ^m when ^m: (static member From: Wire<'a> -> ^m)
    
    /// Maps a function over a computation.
    [<CompiledName ("Map")>]
    val map: f:('a -> 'b) -> comp:Wire<'a> -> Wire<'b>
    
    /// Composes a computation.
    [<CompiledName ("MapC")>]
    val mapc: f:('a -> Eventive<'b>) -> comp:Wire<'a> -> Wire<'b>

    /// Transforms a computation.
    [<CompiledName ("Ap")>]
    val ap: f:Eventive<'a -> 'b> -> comp:Wire<'a> -> Wire<'b>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift2")>]
    val lift2: f:('a -> 'b -> 'c) -> comp1:Wire<'a> -> comp2:Wire<'b> -> Wire<'c>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift3")>]
    val lift3: f:('a -> 'b -> 'c -> 'd) -> comp1:Wire<'a> -> comp2:Wire<'b> -> comp3:Wire<'c> -> Wire<'d>

    /// Promotes a function to computations.
    [<CompiledName ("Lift4")>]
    val lift4: f:('a -> 'b -> 'c -> 'd -> 'e) -> comp1:Wire<'a> -> comp2:Wire<'b> -> comp3:Wire<'c> -> comp4:Wire<'d> -> Wire<'e>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift5")>]
    val lift5: f:('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> comp1:Wire<'a> -> comp2:Wire<'b> -> comp3:Wire<'c> -> comp4:Wire<'d> -> comp5:Wire<'e> -> Wire<'f>

    /// Takes two computations and creates a new computation of the corresponding pairs.  
    [<CompiledName ("Zip")>]
    val zip: comp1:Wire<'a> -> comp2:Wire<'b> -> Wire<'a * 'b>
    
    /// Takes three computations and creates a new computation of the corresponding triples.
    [<CompiledName ("Zip3")>]
    val zip3: comp1:Wire<'a> -> comp2:Wire<'b> -> comp3:Wire<'c> -> Wire<'a * 'b * 'c>
        
    /// Takes a sequence of computations and creates a new computation of the corresponding sequences.
    [<CompiledName ("OfEnumerable")>]
    val ofSeq: comps:seq<Wire<'b>> -> Wire<seq<'b>>
    
    /// Takes a list of computations and creates a new computation of the corresponding lists.
    [<CompiledName ("OfList")>]
    val ofList: comps:Wire<'b> list -> Wire<'b list>
    
    /// Takes an array of computations and creates a new computation of the corresponding arrays.
    [<CompiledName ("OfArray")>]
    val ofArray: comps:Wire<'b> [] -> Wire<'b []>

    /// Converts a sequence of items to a new computation of the corresponding items.
    [<CompiledName ("FromEnumerable")>]
    val fromSeq: items:seq<'a> -> Wire<'a>

    /// Memoizes the wire so that a new wire would always return the same data within the simulation run regardless of time points at which these data are requested.
    [<CompiledName ("Memo")>]
    val memo: comp:Wire<'a> -> Wire<'a>

    /// Takes the computation of pairs and returns a new computation, where these pairs are splitted.
    [<CompiledName ("Unzip")>]
    val unzip: comp:Wire<'a * 'b> -> Wire<'a> * Wire<'b>

    /// Filters only those data that satisfy the given predicate.
    [<CompiledName ("Filter")>]
    val filter: pred:('a -> bool) -> comp:Wire<'a> -> Wire<'a>

    /// Filters only those data that satisfy the given predicate within the process.
    [<CompiledName ("FilterC")>]
    val filterc: pred:('a -> Eventive<bool>) -> comp:Wire<'a> -> Wire<'a>

    /// Flattens the nested computation.
    [<CompiledName ("Concat")>]
    val concat: comps:Wire<Wire<'a>> -> Wire<'a>

    /// Wraps the two given computations as a single concatenated computation.
    [<CompiledName ("Append")>]
    val append: comp1:Wire<'a> -> comp2:Wire<'a> -> Wire<'a>

    /// Returns a stream that yields one item only.
    [<CompiledName ("Singleton")>]
    val singleton: item:'a -> Wire<'a>

    /// Returns a stream that yields one item only receiving its value from the specified process.
    [<CompiledName ("SingletonC")>]
    val singletonc: comp:Eventive<'a> -> Wire<'a>

    /// Returns an empty stream that neither returns any data nor performs any side effect.
    [<CompiledName ("Empty")>]
    val empty<'a> : Wire<'a>

    /// Returns a wire that supplies values generated by the specified computation.
    [<CompiledName ("Repeat")>]
    val repeat: comp:Eventive<'a> -> Wire<'a>

    /// Returns a stream of the first values.
    [<CompiledName ("Choice1Of2")>]
    val choice1Of2: comp:Wire<Choice<'a, 'b>> -> Wire<'a>
    
    /// Returns a stream of the second values.
    [<CompiledName ("Choice2Of2")>]
    val choice2Of2: comp:Wire<Choice<'a, 'b>> -> Wire<'b>

    /// Replaces the first values in the first stream.
    [<CompiledName ("Replace1Of2")>]
    val replace1Of2: comp1:Wire<Choice<'a, 'b>> -> comp2:Wire<'c> -> Wire<Choice<'c, 'b>>

    /// Replaces the second values in the first stream.
    [<CompiledName ("Replace2Of2")>]
    val replace2Of2: comp1:Wire<Choice<'a, 'b>> -> comp2:Wire<'c> -> Wire<Choice<'a, 'c>>

    /// Partitions the input computation of choice values into two computations.
    [<CompiledName ("PartitionChoice")>]
    val partitionChoice: comp:Wire<Choice<'a, 'b>> -> Wire<'a> * Wire<'b>

    /// Adds the information about the time points at which the values were received.
    [<CompiledName ("ToArrival")>]
    val toArrival: comp:Wire<'a> -> Wire<Arrival<'a>>

    /// Returns the same output values in the same time points not requesting input in such cases so that the time was always different.
    [<CompiledName ("DelayInTime")>]
    val delayInTime: comp:Wire<'a> -> Wire<'a>
