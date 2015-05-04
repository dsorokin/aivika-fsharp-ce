
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

/// It defines how the parent and child computations should be cancelled.
type ContCancellation = 
        /// Cancel the both computations together.
        | CancelTogether
        /// Cancel the child after its parent is cancelled.
        | CancelChildAfterParent
        /// Cancel the parent after its child is cancelled.
        | CancelParentAfterChild
        /// Cancel the computations in isolation.
        | CancelInIsolation

/// The event that occurs within the continuation computation.
type internal ContEvent =
        /// The computation is cancelled.
        | ContCancellationInitiating
        /// The computation is preempted.
        | ContPreemptionBeginning
        /// The computation is proceeded after it was preempted earlier.
        | ContPreemptionEnding

/// It identifies the continuation computation.
[<Sealed>]
type internal ContId =

    /// Creates a cancellation source.
    static member Create: unit -> Simulation<ContId>

    /// Signal when the computation state changes.
    member Signal: Signal<ContEvent> 

    /// Initiates the cancellation process.
    member InitiateCancellation: unit -> Eventive<unit>

    /// Tests whether the cancellation was initiated.
    member CancellationInitiated: Eventive<bool>

    /// Signal when the cancellation is being initiated.
    member CancellationInitiating: Signal<unit>

    /// Tests whether the cancellation is activated.
    member CancellationActivated: bool

    /// Deactivates the cancellation process if needed.
    member DeactivateCancellation: unit -> unit

    /// Preempts the current computation.
    member BeginPreemption: unit -> Eventive<unit>

    /// Try to proceed with the computation after it was preempted earlier. 
    member EndPreemption: unit -> Eventive<unit>

    /// Tests whether the computation is preempted.
    member PreemptionBegun: Eventive<bool>

    /// Signal when the computation is preempted.
    member PreemptionBeginning: Signal<unit>

    /// Signal when the computation is proceeded after it was preempted earlier.
    member PreemptionEnding: Signal<unit>

    /// If the main computation is cancelled then all bound ones will be cancelled too.
    member Bind: others:ContId list -> Eventive<IDisposable>

    /// Connects the parent and child computations.
    member Connect: cancellation:ContCancellation * child:ContId -> Eventive<IDisposable>

/// The auxiliary continuation parameters.
type internal ContParamsAux =
    { /// How to continue in case of raising an exception.
      ECont: exn -> Eventive<unit>;
      /// How to continue in case of cancelling the computation.
      CCont: unit -> Eventive<unit>;
      /// It identifies the computation.
      Id: ContId
    }

/// The continuation parameters.
type internal ContParams<'a> =
    { /// How to continue the main computation.
      Cont: 'a -> Eventive<unit>;
      /// The auxiliary parameters of the computation.
      Aux: ContParamsAux
    }

/// Represents a frozen computation.
[<Sealed; NoEquality; NoComparison>]
type internal FrozenCont<'a> =

    /// Creates a new frozen computation.
    internal new: comp:Eventive<ContParams<'a> option> -> FrozenCont<'a>

    /// Returns the underlying computation.
    member internal Comp: Eventive<ContParams<'a> option>

/// Represents a computation that can suspend at one simulation time point and then resume later at another time point.
[<Sealed; NoEquality; NoComparison>]
type Cont<'a> =

    /// Creates a new computation.
    internal new: f:(ContParams<'a> -> Eventive<unit>) -> Cont<'a>

    /// Returns the underlying function.
    member internal Fun: (ContParams<'a> -> Eventive<unit>)

    /// Lifts a computation.
    static member From: comp:Parameter<'a> -> Cont<'a>

    /// Lifts a computation.
    static member From: comp:Simulation<'a> -> Cont<'a>

    /// Lifts a computation.
    static member From: comp:Dynamics<'a> -> Cont<'a>

    /// Lifts a computation.
    static member From: comp:Eventive<'a> -> Cont<'a>

    /// Lifts a computation.
    static member From: comp:Cont<'a> -> Cont<'a>

/// Contains a function that allows invoking the Cont computation.
[<AutoOpen>]
module internal ContInvoke =

    /// Invokes the computation with the specified continuation parameters.
    val inline invokeCont: conts:ContParams<'a> -> comp:Cont<'a> -> Eventive<unit>

    /// Resumes the computation.
    val internal resumeCont: conts:ContParams<'a> -> value:'a -> Eventive<unit>

    /// Resumes the exception handling.
    val internal resumeECont: conts:ContParams<'a> -> e:exn -> Eventive<unit>

    /// Sleeps until the preempted computation will be reentered.
    val internal sleepCont: conts:ContParams<'a> -> value:'a -> Eventive<unit>

    /// Freezes the computation parameters temporarily.
    val internal freezeCont: conts:ContParams<'a> -> Eventive<FrozenCont<'a>>

    /// Freezes the computation parameters temporarily specifying what should be done when reentering the computation.
    val internal freezeContReentering: conts:ContParams<'a> -> value:'a -> comp:Eventive<unit> -> Eventive<FrozenCont<'a>>

    /// Unfreezes the compution as possible.
    val inline internal unfreezeCont: FrozenCont<'a> -> Eventive<ContParams<'a> option>

    /// Reenters the computation after it was preempted.
    val internal reenterCont: conts:ContParams<'a> -> value:'a -> Eventive<unit>

    /// Substitutes the computation.
    val internal substituteCont: conts:ContParams<'a> -> f:('a -> Eventive<unit>) -> ContParams<'a>

/// Represents a builder for constructing Cont computations.
[<Sealed>]
type internal ContBuilder =

    /// Creates a computation that returns the specified value.
    member Return: value:'a -> Cont<'a>
    
    /// Delegates to the input computation.
    member ReturnFrom: comp:Cont<'a> -> Cont<'a>
    
    /// Creates a computation that binds the specified computation with its continuation.
    member Bind: comp:Cont<'a> * cont:('a -> Cont<'b>) -> Cont<'b>
    
    /// Creates a computation that runs the specified one.
    member Delay: comp:(unit -> Cont<'b>) -> Cont<'b>
    
    /// Creates a computation that just returns ().
    member Zero: unit -> Cont<unit>
    
    /// Creates a computation that runs the first computation and then runs the second one, returning the result of the latter.
    member Combine: comp1:Cont<unit> * comp2:Cont<'a> -> Cont<'a>
    
    /// Creates a computation that enumerates the sequence on demand and runs body for each element.
    member For: comp:seq<'a> * body:('a -> Cont<unit>) -> Cont<unit>
    
    /// Creates a computation that runs the specified computation repeatedly until the provided guard becomes false.
    member While: pred:(unit -> bool) * body:Cont<unit> -> Cont<unit>
    
    /// Creates a computation that allows using the disposable resource.
    member Using: resource:'a * body:('a -> Cont<'b>) -> Cont<'b> when 'a :> IDisposable
    
    /// Creates a computation with the finalization part.
    member TryFinally: comp:Cont<'a> * finalizer:(unit -> unit) -> Cont<'a>
    
    /// Creates a computation with the exception handling.
    member TryWith: comp:Cont<'a> * handler:(exn -> Cont<'a>) -> Cont<'a>

/// Contains a builder of the Cont computation.
[<AutoOpen>]
module internal ContWorkflow =     

    /// The builder of the Cont computation.
    val cont: ContBuilder

/// The module contains useful functions for working with the Cont computations.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module internal Cont =

    /// Runs a computation.
    [<CompiledName ("Run")>]
    val run: comp:Cont<'a> 
        -> cont1:('a -> Eventive<unit>)
        -> cont2:(exn -> Eventive<unit>)
        -> cont3:(unit -> Eventive<unit>)
        -> cid:ContId
        -> Eventive<unit>

    /// Lifts a computation.
    [<CompiledName ("Lift")>]
    val inline lift: comp:Cont<'a> -> ^m when ^m: (static member From: Cont<'a> -> ^m)

    /// Flattens the nested computation.
    [<CompiledName ("Concat")>]
    val concat: comp:Cont<Cont<'a>> -> Cont<'a>
    
    /// Maps a function over a computation.
    [<CompiledName ("Map")>]
    val map: f:('a -> 'b) -> comp:Cont<'a> -> Cont<'b>
    
    /// Transforms a computation.
    [<CompiledName ("Ap")>]
    val ap: f:Cont<'a -> 'b> -> comp:Cont<'a> -> Cont<'b>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift2")>]
    val lift2: f:('a -> 'b -> 'c) -> comp1:Cont<'a> -> comp2:Cont<'b> -> Cont<'c>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift3")>]
    val lift3: f:('a -> 'b -> 'c -> 'd) -> comp1:Cont<'a> -> comp2:Cont<'b> -> comp3:Cont<'c> -> Cont<'d>

    /// Promotes a function to computations.
    [<CompiledName ("Lift4")>]
    val lift4: f:('a -> 'b -> 'c -> 'd -> 'e) -> comp1:Cont<'a> -> comp2:Cont<'b> -> comp3:Cont<'c> -> comp4:Cont<'d> -> Cont<'e>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift5")>]
    val lift5: f:('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> comp1:Cont<'a> -> comp2:Cont<'b> -> comp3:Cont<'c> -> comp4:Cont<'d> -> comp5:Cont<'e> -> Cont<'f>

    /// Takes two computations and creates a new computation of the corresponding pairs.  
    [<CompiledName ("Zip")>]
    val zip: comp1:Cont<'a> -> comp2:Cont<'b> -> Cont<'a * 'b>
    
    /// Takes three computations and creates a new computation of the corresponding triples.
    [<CompiledName ("Zip3")>]
    val zip3: comp1:Cont<'a> -> comp2:Cont<'b> -> comp3:Cont<'c> -> Cont<'a * 'b * 'c>
    
    /// Takes a list of computations and creates a new computation of the corresponding lists.
    [<CompiledName ("OfList")>]
    val ofList: comps:Cont<'b> list -> Cont<'b list>
    
    /// Takes an array of computations and creates a new computation of the corresponding arrays.
    [<CompiledName ("OfArray")>]
    val ofArray: comps:Cont<'b> [] -> Cont<'b []>
        
    /// Takes a sequence of computations and creates a new computation of the corresponding sequences.
    [<CompiledName ("OfEnumerable")>]
    val ofSeq: comps:#seq<Cont<'b>> -> Cont<seq<'b>>

    /// Executes the specified computations simultaneously within the current computation and returns their results.
    [<CompiledName ("Par")>]
    val par: comps:(Cont<'a> * ContId) list -> Cont<'a list>

    /// Executes the specified computations simultaneously within the current computation and ignores their results.
    [<CompiledName ("Par_")>]
    val par_: comps:(Cont<'a> * ContId) list -> Cont<unit>

    /// Reruns the computation with the specified identifier.
    [<CompiledName ("ReRun")>]
    val rerun: comp:Cont<'a> -> cid:ContId -> Cont<'a>

    /// Runs another computation simultaneously but binds the computation identifiers.
    [<CompiledName ("Spawn")>]
    val spawn: cancellation:ContCancellation -> comp:Cont<unit> -> cid:ContId -> Cont<unit>

    /// Awaits the specified signal and then resumes the computation.
    [<CompiledName ("Await")>]
    val await: signal:Signal<'a> -> Cont<'a>
