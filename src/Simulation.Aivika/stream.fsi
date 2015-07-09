
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

/// Represents the stream item at the current time point.
type internal StreamItem<'a> =
    | StreamNil
    | StreamCons of 'a * Stream<'a>

/// Represents a stream of values that are received sequentially in simulation time points.
and [<Sealed; NoEquality; NoComparison>] Stream<'a> =

    /// Creates a new computation.
    internal new: comp:Proc<StreamItem<'a>> -> Stream<'a>

    /// Returns the underlying computation.
    member internal Proc: Proc<StreamItem<'a>>

    /// Lifts a computation.
    static member From: comp:Parameter<unit> -> Stream<'a>

    /// Lifts a computation.
    static member From: comp:Simulation<unit> -> Stream<'a>

    /// Lifts a computation.
    static member From: comp:Dynamics<unit> -> Stream<'a>

    /// Lifts a computation.
    static member From: comp:Eventive<unit> -> Stream<'a>

    /// Lifts a computation.
    static member From: comp:Wire<'a> -> Stream<'a>

    /// Lifts a computation.
    static member From: comp:Cont<unit> -> Stream<'a>

    /// Lifts a computation.
    static member From: comp:Proc<unit> -> Stream<'a>

    /// Lifts a computation.
    static member From: comp:Stream<'a> -> Stream<'a>

/// Contains a function that allows invoking the Proc computation.
[<AutoOpen>]
module internal StreamInvoke =

    /// Invokes the computation.
    val inline invokeStream: comp:Stream<'a> -> Proc<StreamItem<'a>>

/// Represents a builder for constructing Stream computations.
[<Sealed>]
type StreamBuilder =

    /// Creates a computation that yields the specified value.
    member Yield: value:'a -> Stream<'a>
    
    /// Delegates to the input computation.
    member YieldFrom: comp:Stream<'a> -> Stream<'a>

    /// Creates a computation that yields nothing.
    member Return: value:'u -> Stream<'a>
    
    /// Creates a computation that performs side effect but yields nothing.
    member ReturnFrom: comp:Proc<unit> -> Stream<'a>

    /// Creates a computation that binds the specified computation with its continuation.
    member Bind: comp:Proc<'a> * cont:('a -> Stream<'b>) -> Stream<'b>
    
    /// Creates a computation that runs the specified one.
    member Delay: comp:(unit -> Stream<'b>) -> Stream<'b>
    
    /// Creates a computation that does nothing.
    member Zero<'a> : unit -> Stream<'a>
    
    /// Creates a computation that combines the both input computations.
    member Combine: comp1:Stream<'a> * comp2:Stream<'a> -> Stream<'a>

    /// Creates a computation that enumerates the stream on demand and runs body for each element.
    member For: comp:Stream<'a> * body:('a -> Stream<'b>) -> Stream<'b>

    /// Creates a computation that enumerates the sequence on demand and runs body for each element.
    member For: comp:seq<'a> * body:('a -> Stream<'b>) -> Stream<'b>
    
    /// Creates a computation that runs the specified computation repeatedly until the provided guard becomes false.
    member While: pred:(unit -> bool) * body:Stream<'b> -> Stream<'b>
    
    /// Creates a computation that allows using the disposable resource.
    member Using: resource:'a * body:('a -> Stream<'b>) -> Stream<'b> when 'a :> IDisposable
    
    /// Creates a computation with the finalization part.
    member TryFinally: comp:Stream<'a> * finalizer:(unit -> unit) -> Stream<'a>
    
    /// Creates a computation with the exception handling.
    member TryWith: comp:Stream<'a> * handler:(exn -> Stream<'a>) -> Stream<'a>

/// Contains a builder of the Stream computation.
[<AutoOpen>]
module StreamWorkflow =     

    /// The builder of the Stream computation.
    val stream: StreamBuilder

/// This module defines some extensions for the Proc computation builder.
[<AutoOpen>]
module ProcBuilderExtensions =

    type ProcBuilder with

        /// Creates a computation that enumerates the stream on demand and runs body for each element.
        member For: comp:Stream<'a> * body:('a -> Proc<unit>) -> Proc<unit>
    
/// The module contains useful functions for working with the streams of data.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Stream =

    /// Lifts a computation.
    [<CompiledName ("Lift")>]
    val inline lift: comp:Stream<'a> -> ^m when ^m: (static member From: Stream<'a> -> ^m)
    
    /// Maps a function over a computation.
    [<CompiledName ("Map")>]
    val map: f:('a -> 'b) -> comp:Stream<'a> -> Stream<'b>
    
    /// Composes a computation.
    [<CompiledName ("MapC")>]
    val mapc: f:('a -> Proc<'b>) -> comp:Stream<'a> -> Stream<'b>
    
    /// Transforms a computation.
    [<CompiledName ("Ap")>]
    val ap: f:Proc<'a -> 'b> -> comp:Stream<'a> -> Stream<'b>
      
    /// Promotes a function to computations.
    [<CompiledName ("Lift2")>]
    val lift2: f:('a -> 'b -> 'c) -> comp1:Stream<'a> -> comp2:Stream<'b> -> Stream<'c>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift3")>]
    val lift3: f:('a -> 'b -> 'c -> 'd) -> comp1:Stream<'a> -> comp2:Stream<'b> -> comp3:Stream<'c> -> Stream<'d>

    /// Promotes a function to computations.
    [<CompiledName ("Lift4")>]
    val lift4: f:('a -> 'b -> 'c -> 'd -> 'e) -> comp1:Stream<'a> -> comp2:Stream<'b> -> comp3:Stream<'c> -> comp4:Stream<'d> -> Stream<'e>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift5")>]
    val lift5: f:('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> comp1:Stream<'a> -> comp2:Stream<'b> -> comp3:Stream<'c> -> comp4:Stream<'d> -> comp5:Stream<'e> -> Stream<'f>

    /// Takes two computations and creates a new computation of the corresponding pairs.  
    [<CompiledName ("Zip")>]
    val zip: comp1:Stream<'a> -> comp2:Stream<'b> -> Stream<'a * 'b>
    
    /// Takes three computations and creates a new computation of the corresponding triples.
    [<CompiledName ("Zip3")>]
    val zip3: comp1:Stream<'a> -> comp2:Stream<'b> -> comp3:Stream<'c> -> Stream<'a * 'b * 'c>
        
    /// Takes a sequence of computations and creates a new computation of the corresponding sequences.
    [<CompiledName ("OfEnumerable")>]
    val ofSeq: comps:seq<Stream<'b>> -> Stream<seq<'b>>
    
    /// Takes a list of computations and creates a new computation of the corresponding lists.
    [<CompiledName ("OfList")>]
    val ofList: comps:Stream<'b> list -> Stream<'b list>
    
    /// Takes an array of computations and creates a new computation of the corresponding arrays.
    [<CompiledName ("OfArray")>]
    val ofArray: comps:Stream<'b> [] -> Stream<'b []>

    /// Converts a sequence of items to a new computation of the corresponding items.
    [<CompiledName ("FromEnumerable")>]
    val fromSeq: items:seq<'a> -> Stream<'a>

    /// Memoizes the stream so that a new stream would always return the same data at the same time points within the simulation run.
    [<CompiledName ("Memo")>]
    val memo: comp:Stream<'a> -> Stream<'a>

    /// Takes two streams of data and creates a new computation of pairs, where the data are received simultaneously for each pair.
    [<CompiledName ("ParZip")>]
    val parZip: comp1:Stream<'a> -> comp2:Stream<'b> -> Stream<'a * 'b>

    /// Takes three streams of data and creates a new computation of triples, where the data are received simultaneously for each triple.
    [<CompiledName ("ParZip3")>]
    val parZip3: comp1:Stream<'a> -> comp2:Stream<'b> -> comp3:Stream<'c> -> Stream<'a * 'b * 'c>

    /// Takes the computation of pairs and returns a new computation, where these pairs are splitted.
    [<CompiledName ("Unzip")>]
    val unzip: comp:Stream<'a * 'b> -> Stream<'a> * Stream<'b>

    /// Filters only those data that satisfy the given predicate.
    [<CompiledName ("Filter")>]
    val filter: pred:('a -> bool) -> comp:Stream<'a> -> Stream<'a>

    /// Filters only those data that satisfy the given predicate within the process.
    [<CompiledName ("FilterC")>]
    val filterc: pred:('a -> Proc<bool>) -> comp:Stream<'a> -> Stream<'a>

    /// Flattens the nested computation.
    [<CompiledName ("Concat")>]
    val concat: comp:Stream<Stream<'a>> -> Stream<'a>

    /// Wraps the two given computations as a single concatenated computation.
    [<CompiledName ("Append")>]
    val append: comp1:Stream<'a> -> comp2:Stream<'a> -> Stream<'a>

    /// Returns a stream that yields one item only.
    [<CompiledName ("Singleton")>]
    val singleton: item:'a -> Stream<'a>

    /// Returns a stream that yields one item only receiving its value from the specified process.
    [<CompiledName ("SingletonC")>]
    val singletonc: comp:Proc<'a> -> Stream<'a>

    /// Returns an empty stream that neither returns any data nor performs any side effect.
    [<CompiledName ("Empty")>]
    val empty<'a> : Stream<'a>

    /// Returns an undefined stream that never yields any data and which process remains passivated.
    [<CompiledName ("Never")>]
    val never<'a> : Stream<'a>

    /// Returns an infinite stream of values generated by the specified process.
    [<CompiledName ("Repeat")>]
    val repeat: comp:Proc<'a> -> Stream<'a>

    /// Splits the input stream into the specified number of output streams using the given strategy to enqueue the concurrent output requests (demultiplexing).
    [<CompiledName ("SplitQueueing")>]
    val splitQueueing: strat:#IQueueStrategy -> number:int -> stream:Stream<'a> -> Stream<'a> list

    /// Splits the input stream into a list of output streams using the specified priorities and the corresponding strategy to enqueue the concurrent output requests (demultiplexing).
    [<CompiledName ("SplitPrioritising")>]
    val splitPrioritising: strat:#IQueueStrategy -> priorities:Stream<Priority> list -> stream:Stream<'a> -> Stream<'a> list

    /// Splits the input stream into the specified number of output streams applying strategy FCFS (First Come - First Served).
    [<CompiledName ("Split")>]
    val split: number:int -> stream:Stream<'a> -> Stream<'a> list

    /// Splits the input stream into two output streams applying strategy FCFS (First Come - First Served).
    [<CompiledName ("Split2")>]
    val split2: stream:Stream<'a> -> Stream<'a> * Stream<'a>

    /// Splits the input stream into three output streams applying strategy FCFS (First Come - First Served).
    [<CompiledName ("Split3")>]
    val split3: stream:Stream<'a> -> Stream<'a> * Stream<'a> * Stream<'a>

    /// Merges the input streams producing an output stream and using the specified strategy to enqueue the concurrent input data (multiplexing).
    [<CompiledName ("MergeQueueing")>]
    val mergeQueueing: strat:#IQueueStrategy -> streams:Stream<'a> list -> Stream<'a>

    /// Merges the prioritised streams producing an output stream and using the specified strategy to enqueue the concurrent input data (multiplexing).
    [<CompiledName ("MergePrioritising")>]
    val mergePrioritising: strat:#IQueueStrategy -> streams:Stream<Priority * 'a> list -> Stream<'a>
 
    /// Merges the input streams producing an output stream applying strategy FCFS (First Come - First Served).
    [<CompiledName ("Merge")>]
    val merge: streams:Stream<'a> list -> Stream<'a>

    /// Merges two input streams producing an output stream applying strategy FCFS (First Come - First Served).
    [<CompiledName ("Merge2")>]
    val merge2: stream1:Stream<'a> -> stream2:Stream<'a> -> Stream<'a>

    /// Merges three input streams producing an output stream applying strategy FCFS (First Come - First Served).
    [<CompiledName ("Merge3")>]
    val merge3: stream1:Stream<'a> -> stream2:Stream<'a> -> stream3:Stream<'a> -> Stream<'a>

    /// Consumes the input stream producing the specified action. 
    [<CompiledName ("Consume")>]
    val consume: consumer:('a -> Proc<unit>) -> stream:Stream<'a> -> Proc<unit>

    /// Sinks the stream consuming all its items.
    [<CompiledName ("Sink")>]
    val sink: stream:Stream<'a> -> Proc<unit>

    /// Creates a stream that will use the specified process identifier.
    [<CompiledName ("UsingId")>]
    val usingId: pid:ProcId -> stream:Stream<'a> -> Stream<'a>

    /// Returns a stream that requests for data simultaneously, waits for their receipt, yields them as a sequence and then repeats again.
    [<CompiledName ("Par")>]
    val par: streams:Stream<'a> list -> Stream<'a list>

    /// Returns a stream of the first values.
    [<CompiledName ("Choice1Of2")>]
    val choice1Of2: stream:Stream<Choice<'a, 'b>> -> Stream<'a>
    
    /// Returns a stream of the second values.
    [<CompiledName ("Choice2Of2")>]
    val choice2Of2: stream:Stream<Choice<'a, 'b>> -> Stream<'b>

    /// Replaces the first values in the first stream.
    [<CompiledName ("Replace1Of2")>]
    val replace1Of2: stream1:Stream<Choice<'a, 'b>> -> stream2:Stream<'c> -> Stream<Choice<'c, 'b>>

    /// Replaces the second values in the first stream.
    [<CompiledName ("Replace2Of2")>]
    val replace2Of2: stream1:Stream<Choice<'a, 'b>> -> stream2:Stream<'c> -> Stream<Choice<'a, 'c>>

    /// Partitions the stream of choice values into two streams.
    [<CompiledName ("PartitionChoice")>]
    val partitionChoice: stream:Stream<Choice<'a, 'b>> -> Stream<'a> * Stream<'b>

    /// Adds the information about the time points at which the values were received.
    [<CompiledName ("ToArrival")>]
    val toArrival: stream:Stream<'a> -> Stream<Arrival<'a>>

    /// Returns an infinite stream of arrivals by the specified delay and the corresponding value that are calculated again and again.
    [<CompiledName ("RandomArrival")>]
    val randomArrival: comp:Parameter<float * 'a> -> Stream<Arrival<'a>>
    
    /// Returns a stream with delays distributed uniformly with the specified minimum and maximum.
    [<CompiledName ("RandomUniform")>]
    val randomUniform: minimum:float -> maximum:float -> Stream<Arrival<float>>
    
    /// Returns a stream with integer delays distributed uniformly with the specified minimum and maximum.
    [<CompiledName ("RandomUniformInt")>]
    val randomUniformInt: minimum:int -> maximum:int -> Stream<Arrival<int>>
    
    /// Returns a stream with delays distributed normally with the specified mean and deviation.
    [<CompiledName ("RandomNormal")>]
    val randomNormal: mean:float -> deviation:float -> Stream<Arrival<float>>    
    
    /// Returns a stream with delays distributed exponentially with the specified mean (the reciprocal of the rate).
    [<CompiledName ("RandomExponential")>]
    val randomExponential: mean:float -> Stream<Arrival<float>>
    
    /// Returns a stream with delays having the Erlang distribution with the specified scale (the reciprocal of the rate) and integer shape.
    [<CompiledName ("RandomErlang")>]
    val randomErlang: beta:float -> m:int -> Stream<Arrival<float>>
    
    /// Returns a stream with delays having the Poisson distribution with the specified mean.
    [<CompiledName ("RandomPoisson")>]
    val randomPoisson: mean:float -> Stream<Arrival<int>>
    
    /// Returns a stream with delays having the binomial distribution with the specified probability and number of trials.
    [<CompiledName ("RandomBinomial")>]
    val randomBinomial: prob:float -> trials:int -> Stream<Arrival<int>>

    /// Prefetches the input stream requesting for one more data item in advance while the last received item is not yet fully processed in the chain of streams, usually by the processors.
    [<CompiledName ("Prefetch")>]
    val prefetch: stream:Stream<'a> -> Stream<'a>

    /// Returns a computation of the signal that triggers values from the specified stream, each time the next value of the stream is received.
    [<CompiledName ("ToSignal")>]
    val toSignal: stream:Stream<'a> -> Proc<Signal<'a>>

    /// Returns a stream of values triggered by the specified signal.
    [<CompiledName ("OfSignal")>]
    val ofSignal: signal:Signal<'a> -> Proc<Stream<'a>>

    /// Returns the prefix of the stream of the specified length.
    [<CompiledName ("Take")>]
    val take: n:int -> stream:Stream<'a> -> Stream<'a>

    /// Returns the longest prefix of the stream of elements that satisfy the predicate.
    [<CompiledName ("TakeWhile")>]
    val takeWhile: pred:('a -> bool) -> Stream<'a> -> Stream<'a>

    /// Returns the longest prefix of the stream of elements that satisfy the computation.
    [<CompiledName ("TakeWhileC")>]
    val takeWhileC: pred:('a -> Proc<bool>) -> Stream<'a> -> Stream<'a>

    /// Returns the suffix of the stream after the specified first elements.
    [<CompiledName ("Drop")>]
    val drop: n:int -> stream:Stream<'a> -> Stream<'a>

    /// Returns the suffix of the stream remaining after the first elements that satisfy the specified predicate.
    [<CompiledName ("DropWhile")>]
    val dropWhile: pred:('a -> bool) -> Stream<'a> -> Stream<'a>

    /// Returns the suffix of the stream remaining after the first elements that satisfy the specified computation.
    [<CompiledName ("DropWhileC")>]
    val dropWhileC: pred:('a -> Proc<bool>) -> Stream<'a> -> Stream<'a>
