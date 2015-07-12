
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

/// Represents a discontinuous process identifier.
[<Sealed>]
type ProcId

type ProcId with

    /// Creates a new process identifier within the computation.
    static member Create: unit -> Simulation<ProcId>

/// Represents a discontinuous process that can suspend at one simulation time point and then resume later at another time point.
[<Sealed; NoEquality; NoComparison>]
type Proc<'a> =

    /// Creates a new computation.
    internal new: f:(ProcId -> Cont<'a>) -> Proc<'a>

    /// Returns the underlying function.
    member internal Fun: (ProcId -> Cont<'a>)

    /// Lifts a computation.
    static member From: comp:Parameter<'a> -> Proc<'a>

    /// Lifts a computation.
    static member From: comp:Simulation<'a> -> Proc<'a>

    /// Lifts a computation.
    static member From: comp:Dynamics<'a> -> Proc<'a>

    /// Lifts a computation.
    static member From: comp:Eventive<'a> -> Proc<'a>

    /// Lifts a computation.
    static member From: comp:Cont<'a> -> Proc<'a>

    /// Lifts a computation.
    static member From: comp:Proc<'a> -> Proc<'a>

/// Contains a function that allows invoking the Proc computation.
[<AutoOpen>]
module internal ProcInvoke =

    /// Invokes the computation with the specified identifier.
    val inline invokeProc: pid:ProcId -> comp:Proc<'a> -> Cont<'a>

/// Represents a builder for constructing Proc computations.
[<Sealed>]
type ProcBuilder =

    /// Creates a computation that returns the specified value.
    member Return: value:'a -> Proc<'a>
    
    /// Delegates to the input computation.
    member ReturnFrom: comp:Proc<'a> -> Proc<'a>
    
    /// Creates a computation that binds the specified computation with its continuation.
    member Bind: comp:Proc<'a> * cont:('a -> Proc<'b>) -> Proc<'b>
    
    /// Creates a computation that runs the specified one.
    member Delay: comp:(unit -> Proc<'b>) -> Proc<'b>
    
    /// Creates a computation that just returns ().
    member Zero: unit -> Proc<unit>
    
    /// Creates a computation that runs the first computation and then runs the second one, returning the result of the latter.
    member Combine: comp1:Proc<unit> * comp2:Proc<'a> -> Proc<'a>
    
    /// Creates a computation that enumerates the sequence on demand and runs body for each element.
    member For: comp:seq<'a> * body:('a -> Proc<unit>) -> Proc<unit>
    
    /// Creates a computation that runs the specified computation repeatedly until the provided guard becomes false.
    member While: pred:(unit -> bool) * comp:Proc<unit> -> Proc<unit>
    
    /// Creates a computation that allows using the disposable resource.
    member Using: resource:'a * body:('a -> Proc<'b>) -> Proc<'b> when 'a :> IDisposable
    
    /// Creates a computation with the finalization part.
    member TryFinally: comp:Proc<'a> * finalizer:(unit -> unit) -> Proc<'a>
    
    /// Creates a computation with the exception handling.
    member TryWith: comp:Proc<'a> * handler:(exn -> Proc<'a>) -> Proc<'a>

/// Contains a builder of the Proc computation.
[<AutoOpen>]
module ProcWorkflow =     

    /// The builder of the Proc computation.
    val proc: ProcBuilder

/// The module contains useful functions for working with the discontinuous processes.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Proc =

    /// Lifts a computation.
    [<CompiledName ("Lift")>]
    val inline lift: comp:Proc<'a> -> ^m when ^m: (static member From: Proc<'a> -> ^m)

    /// Flattens the nested computation.
    [<CompiledName ("Concat")>]
    val concat: comp:Proc<Proc<'a>> -> Proc<'a>
    
    /// Maps a function over a computation.
    [<CompiledName ("Map")>]
    val map: f:('a -> 'b) -> comp:Proc<'a> -> Proc<'b>
    
    /// Transforms a computation.
    [<CompiledName ("Ap")>]
    val ap: f:Proc<'a -> 'b> -> comp:Proc<'a> -> Proc<'b>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift2")>]
    val lift2: f:('a -> 'b -> 'c) -> comp1:Proc<'a> -> comp2:Proc<'b> -> Proc<'c>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift3")>]
    val lift3: f:('a -> 'b -> 'c -> 'd) -> comp1:Proc<'a> -> comp2:Proc<'b> -> comp3:Proc<'c> -> Proc<'d>

    /// Promotes a function to computations.
    [<CompiledName ("Lift4")>]
    val lift4: f:('a -> 'b -> 'c -> 'd -> 'e) -> comp1:Proc<'a> -> comp2:Proc<'b> -> comp3:Proc<'c> -> comp4:Proc<'d> -> Proc<'e>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift5")>]
    val lift5: f:('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> comp1:Proc<'a> -> comp2:Proc<'b> -> comp3:Proc<'c> -> comp4:Proc<'d> -> comp5:Proc<'e> -> Proc<'f>

    /// Takes two computations and creates a new computation of the corresponding pairs.   
    [<CompiledName ("Zip")>]
    val zip: comp1:Proc<'a> -> comp2:Proc<'b> -> Proc<'a * 'b>
    
    /// Takes three computations and creates a new computation of the corresponding triples.
    [<CompiledName ("Zip3")>]
    val zip3: comp1:Proc<'a> -> comp2:Proc<'b> -> comp3:Proc<'c> -> Proc<'a * 'b * 'c>
    
    /// Takes a list of computations and creates a new computation of the corresponding lists.
    [<CompiledName ("OfList")>]
    val ofList: comps:Proc<'b> list -> Proc<'b list>
    
    /// Takes an array of computations and creates a new computation of the corresponding arrays.
    [<CompiledName ("OfArray")>]
    val ofArray: comps:Proc<'b> [] -> Proc<'b []>
        
    /// Takes a sequence of computations and creates a new computation of the corresponding sequences.
    [<CompiledName ("OfEnumerable")>]
    val ofSeq: comps:#seq<Proc<'b>> -> Proc<seq<'b>>

    /// Holds the process for the specified time period.
    [<CompiledName ("Hold")>]
    val hold: dt:Time -> Proc<unit>

    /// Interrupts a process with the specified identifier if the process is held.
    [<CompiledName ("Interrupt")>]
    val interrupt: pid:ProcId -> Eventive<unit>

    /// Tests whether the process with the specified identifier was interrupted.
    [<CompiledName ("IsInterrupted")>]
    val isInterrupted: pid:ProcId -> Eventive<bool>

    /// Passivates the process until another activity will reactivate it explicitly.
    [<CompiledName ("Passivate")>]
    val passivate: Proc<unit>

    /// Reactivates the passivated process with the specified identifier.
    [<CompiledName ("Reactivate")>]
    val reactivate: pid:ProcId -> Eventive<unit>

    /// Tests whether the process with the specified identifier is passivated.
    [<CompiledName ("IsPassivated")>]
    val isPassivated: pid:ProcId -> Eventive<bool>

    /// Computation that returns the identifier of the current process.
    [<CompiledName ("Id")>]
    val id: Proc<ProcId>

    /// Creates an uniqueue process identifier.
    [<CompiledName ("CreateId")>]
    val createId: Simulation<ProcId>

    /// Allows calling the specified sub-process with another identifier.
    [<CompiledName ("UsingId")>]
    val usingId: pid:ProcId -> comp:Proc<'a> -> Proc<'a>

    /// Cancels the process with the specified identifier.
    [<CompiledName ("CancelUsingId")>]
    val cancelUsingId: pid:ProcId -> Eventive<unit>
    
    /// The process cancels itself.
    [<CompiledName ("Cancel")>]
    val cancel<'a> : Proc<'a>

    /// Tests whether the process with the specified identifier is cancelled.
    [<CompiledName ("IsCancelled")>]
    val isCancelled: pid:ProcId -> Eventive<bool>

    /// Returns a signal that notifies about cancellation of the process with the specified identifier.
    [<CompiledName ("Cancelling")>]
    val cancelling: pid:ProcId -> Signal<unit>

    /// Registers a handler that will be invoked in case of cancelling the current process.
    [<CompiledName ("WhenCancelling")>]
    val whenCancelling: handler:Eventive<unit> -> Proc<unit>

    /// Preempts a process with the specified identifier.
    [<CompiledName ("BeginPreemption")>]
    val internal beginPreemption: pid:ProcId -> Eventive<unit>

    /// Tries to proceed with a process that has the specified identifier after the process was preempted earlier.
    [<CompiledName ("EndPreemption")>]
    val internal endPreemption: pid:ProcId -> Eventive<unit>

    /// Returns a signal that notifies about preempting a process with the specified identifier.
    [<CompiledName ("PreemptionBeginning")>]
    val preemptionBeginning: pid:ProcId -> Signal<unit>

    /// Returns a signal that notifies about proceeding with a process that has the specified identifier after the process was preempted earlier.
    [<CompiledName ("PreemptionEnding")>]
    val preemptionEnding: pid:ProcId -> Signal<unit>

    /// Runs a new process with the specified identifier.
    [<CompiledName ("RunUsingId")>]
    val runUsingId: pid:ProcId -> comp:Proc<unit> -> Eventive<unit>

    /// Runs a new process for which the identifier is generated automatically.
    [<CompiledName ("Run")>]
    val run: comp:Proc<unit> -> Eventive<unit>

    /// Runs a new process using the specified identifier in the start time point.
    [<CompiledName ("RunInStartTimeUsingId")>]
    val runInStartTimeUsingId: pid:ProcId -> comp:Proc<unit> -> Simulation<unit>

    /// Runs a new process in the start time point.
    [<CompiledName ("RunInStartTime")>]
    val runInStartTime: comp:Proc<unit> -> Simulation<unit>

    /// Runs a new process using the specified identifier in the final time point.
    [<CompiledName ("RunInStopTimeUsingId")>]
    val runInStopTimeUsingId: pid:ProcId -> comp:Proc<unit> -> Simulation<unit>

    /// Runs a new process in the final time point.
    [<CompiledName ("RunInStopTime")>]
    val runInStopTime: comp:Proc<unit> -> Simulation<unit>

    /// Runs a new child process simultaneously using the given identifier and specifying how this child and current parent processes should be cancelled in case of need.
    [<CompiledName ("SpawnUsingIdWith")>]
    val spawnUsingIdWith: cancellation:ContCancellation -> pid:ProcId -> comp:Proc<unit> -> Proc<unit>

    /// Runs a new child process simultaneously specifying how this child and current parent processes should be cancelled in case of need.
    [<CompiledName ("SpawnWith")>]
    val spawnWith: cancellation:ContCancellation -> comp:Proc<unit> -> Proc<unit>

    /// Runs a new child process simultaneously using the given identifier and specifying how this child and current parent processes should be cancelled in case of need.
    [<CompiledName ("SpawnUsingId")>]
    val spawnUsingId: pid:ProcId -> comp:Proc<unit> -> Proc<unit>

    /// Runs a new child process simultaneously specifying how this child and current parent processes should be cancelled in case of need.
    [<CompiledName ("Spawn")>]
    val spawn: comp:Proc<unit> -> Proc<unit>

    /// Enqueues with the given identifier a new process that will be then started at the specified time from the event queue.
    [<CompiledName ("EnqueueUsingId")>]
    val enqueueUsingId: time:Time -> pid:ProcId -> comp:Proc<unit> -> Eventive<unit>

    /// Enqueues a new process that will be then started at the specified time from the event queue.
    [<CompiledName ("Enqueue")>]
    val enqueue: time:Time -> comp:Proc<unit> -> Eventive<unit>

    /// Runs new processes with the specified identifiers simultaneously, waits for their completion and then returns their results.
    [<CompiledName ("ParUsingIds")>]
    val parUsingIds: comps:(ProcId * Proc<'a>) list -> Proc<'a list>

    /// Runs new processes simultaneously, waits for their completion and then returns their results.
    [<CompiledName ("Par")>]
    val par: comps:Proc<'a> list -> Proc<'a list>

    /// Runs new processes with the specified identifiers simultaneously, waits for their completion but then ignores their results.
    [<CompiledName ("ParUsingIds_")>]
    val parUsingIds_: comps:(ProcId * Proc<'a>) list -> Proc<unit>

    /// Runs new processes simultaneously, waits for their completion but then ignores their results.
    [<CompiledName ("Par_")>]
    val par_: comps:Proc<'a> list -> Proc<unit>

    /// Awaits the signal suspending the current process.
    [<CompiledName ("Await")>]
    val await: signal:Signal<'a> -> Proc<'a>

    /// Memoizes the specified process so that the resulting process would always return the same value within the simulation run.
    [<CompiledName ("Memo")>]
    val memo: comp:Proc<'a> -> Proc<'a>

    /// Runs two computations simultaneously, waits for their completion and then returns their results as a pair within a new computation.
    [<CompiledName ("ParZip")>]
    val parZip: comp1:Proc<'a> -> comp2:Proc<'b> -> Proc<'a * 'b>

    /// Runs three computations simultaneously, waits for their completion and then returns their results as a triple within a new computation.
    [<CompiledName ("ParZip3")>]
    val parZip3: comp1:Proc<'a> -> comp2:Proc<'b> -> comp3:Proc<'c> -> Proc<'a * 'b * 'c>

    /// Takes a computation of pairs and returns a new computation, where these pairs are splitted.
    [<CompiledName ("Unzip")>]
    val unzip: comp:Proc<'a * 'b> -> Proc<'a> * Proc<'b>

    /// Tries to run a child process with the given identifier within the specified timeout, cancelling the child process in case of its exceeding the time limit.
    [<CompiledName ("TimeoutUsingId")>]
    val timeoutUsingId: dt:Time -> pid:ProcId -> comp:Proc<'a> -> Proc<'a option>

    /// Tries to run a child process within the specified timeout, cancelling the child process in case of its exceeding the time limit.
    [<CompiledName ("Timeout")>]
    val timeout: dt:Time -> comp:Proc<'a> -> Proc<'a option>

    /// A computation that never returns result.
    [<CompiledName ("Never")>]
    val never<'a> : Proc<'a>

    /// Computation that holds the process for a random time interval distributed uniformly with the specified minimum and maximum and that returns the interval.
    [<CompiledName ("RandomUniform")>]
    val randomUniform: minimum:float -> maximum:float -> Proc<float>
    
    /// Computation that holds the process for a random time interval distributed uniformly with the specified minimum and maximum.
    [<CompiledName ("RandomUniform_")>]
    val randomUniform_: minimum:float -> maximum:float -> Proc<unit>
    
    /// Computation that holds the process for an integer random time interval distributed uniformly with the specified minimum and maximum and that returns the interval.
    [<CompiledName ("RandomUniformInt")>]
    val randomUniformInt: minimum:int -> maximum:int -> Proc<int>
    
    /// Computation that holds the process for an integer random time interval distributed uniformly with the specified minimum and maximum.
    [<CompiledName ("RandomUniformInt_")>]
    val randomUniformInt_: minimum:int -> maximum:int -> Proc<unit>

    /// Computation that holds the process for a triangular random time interval with the specified minimum, median and maximum and that returns the interval.
    [<CompiledName ("RandomTriangular")>]
    val randomTriangular: minimum:float -> median:float -> maximum:float -> Proc<float>
    
    /// Computation that holds the process for a triangular random time interval with the specified minimum, median and maximum.
    [<CompiledName ("RandomTriangular_")>]
    val randomTriangular_: minimum:float -> median:float -> maximum:float -> Proc<unit>
    
    /// Computation that holds the process for a random time interval distributed normally with the specified mean and deviation and that returns the interval.
    [<CompiledName ("RandomNormal")>]
    val randomNormal: mean:float -> deviation:float -> Proc<float>    
    
    /// Computation that holds the process for a random time interval distributed normally with the specified mean and deviation.
    [<CompiledName ("RandomNormal_")>]
    val randomNormal_: mean:float -> deviation:float -> Proc<unit>    
    
    /// Computation that holds the process for a random time interval distributed exponentially with the specified mean (the reciprocal of the rate) and that returns the interval.
    [<CompiledName ("RandomExponential")>]
    val randomExponential: mean:float -> Proc<float>
    
    /// Computation that holds the process for a random time interval distributed exponentially with the specified mean (the reciprocal of the rate).
    [<CompiledName ("RandomExponential_")>]
    val randomExponential_: mean:float -> Proc<unit>
    
    /// Computation that holds the process for a random time interval having the Erlang distribution with the specified scale (the reciprocal of the rate) and integer shape but then returns the interval.
    [<CompiledName ("RandomErlang")>]
    val randomErlang: beta:float -> m:int -> Proc<float>
    
    /// Computation that holds the process for a random time interval having the Erlang distribution with the specified scale (the reciprocal of the rate) and integer shape.
    [<CompiledName ("RandomErlang_")>]
    val randomErlang_: beta:float -> m:int -> Proc<unit>
    
    /// Computation that holds the process for a random time interval having the Poisson distribution with the specified mean and that returns the interval.
    [<CompiledName ("RandomPoisson")>]
    val randomPoisson: mean:float -> Proc<int>
    
    /// Computation that holds the process for a random time interval having the Poisson distribution with the specified mean.
    [<CompiledName ("RandomPoisson_")>]
    val randomPoisson_: mean:float -> Proc<unit>
    
    /// Computation that holds the process for a random time interval distributed binomially with the specified probability and number of trials but then returns the interval.
    [<CompiledName ("RandomBinomial")>]
    val randomBinomial: prob:float -> trials:int -> Proc<int>

    /// Computation that holds the process for a random time interval distributed binomially with the specified probability and number of trials.
    [<CompiledName ("RandomBinomial_")>]
    val randomBinomial_: prob:float -> trials:int -> Proc<unit>
