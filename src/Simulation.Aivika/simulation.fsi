
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

/// Represents a computation within the simulation run.
[<Sealed; NoEquality; NoComparison>]
type Simulation<'a> =

    /// Creates a new computation.
    internal new: f:(Run -> 'a) -> Simulation<'a>

    /// Returns the underlying function.
    member internal Fun: (Run -> 'a)

    /// Lifts a computation.
    static member From: comp:Parameter<'a> -> Simulation<'a>

    /// Lifts a computation.
    static member From: comp:Simulation<'a> -> Simulation<'a>

/// Contains a function that allows invoking the Simulation computation.
[<AutoOpen>]
module internal SimulationInvoke =

    /// Invokes the computation with the specified run.
    val inline invokeSimulation: r:Run -> comp:Simulation<'a> -> 'a

/// Represents a builder for constructing Simulation computations.
[<Sealed>]
type SimulationBuilder =

    /// Creates a computation that returns the specified value.
    member Return: value:'a -> Simulation<'a>
    
    /// Delegates to the input computation.
    member ReturnFrom: comp:Simulation<'a> -> Simulation<'a>
    
    /// Creates a computation that binds the specified computation with its continuation.
    member Bind: comp:Simulation<'a> * cont:('a -> Simulation<'b>) -> Simulation<'b>
    
    /// Creates a computation that runs the specified one.
    member Delay: comp:(unit -> Simulation<'b>) -> Simulation<'b>
    
    /// Creates a computation that just returns ().
    member Zero: unit -> Simulation<unit>
    
    /// Creates a computation that runs the first computation and then runs the second one, returning the result of the latter.
    member Combine: comp1:Simulation<unit> * comp2:Simulation<'a> -> Simulation<'a>
    
    /// Creates a computation that enumerates the sequence on demand and runs body for each element.
    member For: comp:seq<'a> * body:('a -> Simulation<unit>) -> Simulation<unit>
    
    /// Creates a computation that runs the specified computation repeatedly until the provided guard becomes false.
    member While: pred:(unit -> bool) * comp:Simulation<unit> -> Simulation<unit>
    
    /// Creates a computation that allows using the disposable resource.
    member Using: resource:'a * body:('a -> Simulation<'b>) -> Simulation<'b> when 'a :> IDisposable
    
    /// Creates a computation with the finalization part.
    member TryFinally: comp:Simulation<'a> * finalizer:(unit -> unit) -> Simulation<'a>
    
    /// Creates a computation with the exception handling.
    member TryWith: comp:Simulation<'a> * handler:(exn -> Simulation<'a>) -> Simulation<'a>

/// Contains a builder of the Simulation computation.
[<AutoOpen>]
module SimulationWorkflow =     

    /// The builder of the Simulation computation.
    val simulation: SimulationBuilder

/// The module contains useful functions for working with the Simulation computations.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Simulation =

    /// Runs a computation by the specified specs.
    [<CompiledName ("Run")>]
    val run: specs:Specs -> comp:Simulation<'a> -> 'a
    
    /// Runs a series of computations by the specified specs and number of runs.
    [<CompiledName ("RunSeries")>]
    val runSeries: n:int -> specs:Specs -> comp:Simulation<'a> -> seq<Async<'a>>

    /// Lifts a computation.
    [<CompiledName ("Lift")>]
    val inline lift: comp:Simulation<'a> -> ^m when ^m: (static member From: Simulation<'a> -> ^m)
    
    /// Flattens the nested computation.
    [<CompiledName ("Concat")>]
    val concat: comp:Simulation<Simulation<'a>> -> Simulation<'a>
    
    /// Maps a function over a computation.
    [<CompiledName ("Map")>]
    val map: f:('a -> 'b) -> comp:Simulation<'a> -> Simulation<'b>
    
    /// Transforms a computation.
    [<CompiledName ("Ap")>]
    val ap: f:Simulation<'a -> 'b> -> comp:Simulation<'a> -> Simulation<'b>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift2")>]
    val lift2: f:('a -> 'b -> 'c) -> comp1:Simulation<'a> -> comp2:Simulation<'b> -> Simulation<'c>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift3")>]
    val lift3: f:('a -> 'b -> 'c -> 'd) -> comp1:Simulation<'a> -> comp2:Simulation<'b> -> comp3:Simulation<'c> -> Simulation<'d>

    /// Promotes a function to computations.
    [<CompiledName ("Lift4")>]
    val lift4: f:('a -> 'b -> 'c -> 'd -> 'e) -> comp1:Simulation<'a> -> comp2:Simulation<'b> -> comp3:Simulation<'c> -> comp4:Simulation<'d> -> Simulation<'e>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift5")>]
    val lift5: f:('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> comp1:Simulation<'a> -> comp2:Simulation<'b> -> comp3:Simulation<'c> -> comp4:Simulation<'d> -> comp5:Simulation<'e> -> Simulation<'f>

    /// Takes two computations and creates a new computation of the corresponding pairs.
    [<CompiledName ("Zip")>]
    val zip: comp1:Simulation<'a> -> comp2:Simulation<'b> -> Simulation<'a * 'b>
    
    /// Takes three computations and creates a new computation of the corresponding triples.
    [<CompiledName ("Zip3")>]
    val zip3: comp1:Simulation<'a> -> comp2:Simulation<'b> -> comp3:Simulation<'c> -> Simulation<'a * 'b * 'c>
    
    /// Takes a list of computations and creates a new computation of the corresponding lists.
    [<CompiledName ("OfList")>]
    val ofList: comps:Simulation<'b> list -> Simulation<'b list>
    
    /// Takes an array of computations and creates a new computation of the corresponding arrays.
    [<CompiledName ("OfArray")>]
    val ofArray: comps:Simulation<'b> [] -> Simulation<'b []>
        
    /// Takes a sequence of computations and creates a new computation of the corresponding sequences.
    [<CompiledName ("OfEnumerable")>]
    val ofSeq: comps:#seq<Simulation<'b>> -> Simulation<seq<'b>>
    
    /// Memoizes the computation so that a new computation would always return the same result within the simulation run.
    [<CompiledName ("Memo")>]
    val memo: comp:Simulation<'a> -> Simulation<'a>
    
    /// Implements the if-then-else operator.
    [<CompiledName ("IfThenElse")>]
    val ifThenElse: cond:Simulation<bool> -> thenPart:Simulation<'a> -> elsePart:Simulation<'a> -> Simulation<'a>

/// A root of all simulation exceptions.
type SimulationException =

    inherit Exception

    /// Initializes a new instance.
    new: string -> SimulationException

/// Indicates the abort of simulation.
type SimulationAbort =

    inherit SimulationException

    /// Initializes a new instance.
    new: string -> SimulationAbort
