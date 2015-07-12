
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

/// Represents a processor that takes an input stream and produces an output stream of data.
type Processor<'a, 'b> = Stream<'a> -> Stream<'b>

/// The module contains useful functions for working with the processors.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Processor =

    /// Runs the processor using the specified input stream and producing a new output stream.
    [<CompiledName ("Run")>]
    val run: stream:Stream<'a> -> comp:Processor<'a, 'b> -> Stream<'b>

    /// Creates a processor that will use the specified process identifier.
    [<CompiledName ("UsingId")>]
    val usingId: pid:ProcId -> comp:Processor<'a, 'b> -> Processor<'a, 'b>

    /// Creates a processor that never processes input and never returns output.
    [<CompiledName ("Never")>]
    val never: Processor<'a, 'b>

    /// Creates a new processor by the specified handling function.
    [<CompiledName ("Arr")>]
    val arr: f:('a -> 'b) -> Processor<'a, 'b>

    /// Creates a new processor by the specified handling function that returns the result within the Proc computation.
    [<CompiledName ("ArrC")>]
    val arrc: f:('a -> Proc<'b>) -> Processor<'a, 'b>

    /// Accumulator that outputs a value determined by the supplied function.
    [<CompiledName ("Accum")>]
    val accum: f:('state -> 'a -> Proc<'state * 'b>) -> state:'state -> Processor<'a, 'b> 

    /// Involves the computation with side effect when processing a stream of data.
    [<CompiledName ("Within")>]
    val within: comp:Proc<unit> -> Processor<'a, 'a>

    /// Delay the input by one step using the specified initial value.
    [<CompiledName ("Delay")>]
    val delay: init:'a -> Processor<'a, 'a>

    /// Launches the specified processors simultaneously consuming the same input stream and producing a combined output stream based on the given queue strategies.
    [<CompiledName ("ParQueueing")>]
    val parQueueing: 
            inputStrat:#IQueueStrategy 
                -> outputStrat:#IQueueStrategy 
                -> processors:Processor<'a, 'b> list 
                -> Processor<'a, 'b>

    /// Launches the specified processors simultaneously using the priorities for combining the output based on the given queue strategies.
    [<CompiledName ("ParPrioritisingOutput")>]
    val parPrioritisingOutput: 
            istrat:#IQueueStrategy 
                -> ostrat:#IQueueStrategy 
                -> processors:Processor<'a, Priority * 'b> list 
                -> Processor<'a, 'b>

    /// Launches the specified processors simultaneously using the priorities for consuming the input based on the given queue strategies.
    [<CompiledName ("ParPrioritisingInput")>]
    val parPrioritisingInput: 
            istrat:#IQueueStrategy 
                -> ostrat:#IQueueStrategy 
                -> processors:(Stream<Priority> * Processor<'a, 'b>) list
                -> Processor<'a, 'b>

    /// Launches the specified processors simultaneously using the priorities for consuming the input and combining the output based on the given queue strategies.
    [<CompiledName ("ParPrioritisingInputOutput")>]
    val parPrioritisingInputOutput: 
            istrat:#IQueueStrategy 
                -> ostrat:#IQueueStrategy 
                -> processors:(Stream<Priority> * Processor<'a, Priority * 'b>) list 
                -> Processor<'a, 'b>

    /// Launches the specified processors sequentially.
    [<CompiledName ("Seq")>]
    val seq: processors:Processor<'a, 'a> list -> Processor<'a, 'a>

    /// Launches the specified processors simultaneously consuming the same input stream and producing a combined output stream based on the queue strategy FCFS.
    [<CompiledName ("Par")>]
    val par: processors:Processor<'a, 'b> list -> Processor<'a, 'b>

    /// Creates a buffer processor, where the process from the first argument consumes the input stream, but the stream passed in as the second argument produces an output.
    [<CompiledName ("Buffer")>]
    val buffer: consumer:(Stream<'a> -> Proc<unit>)
                    -> producer:Stream<'b>
                    -> Processor<'a, 'b>

    /// Creates a buffer processor, where the process from the first argument consumes the input stream, but the stream passed in as the second argument produces data which are sent to the specified processor that already decides what elements should be processed by the loop body but what elements are ready for output.
    [<CompiledName ("BufferLoop")>]
    val bufferLoop: consumer:(Stream<'a> -> Stream<'c> -> Proc<unit>)
                        -> producer:Stream<'d>
                        -> cond:Processor<'d, Choice<'e, 'b>>
                        -> body:Processor<'e, 'c>
                        -> Processor<'a, 'b>

    /// Returns a new processor that can be used for modelling the queue.
    [<CompiledName ("Queue")>]
    val queue: enqueue:('a -> Proc<unit>) 
                -> dequeue:Proc<'b>
                -> Processor<'a, 'b>

    /// Returns a new processor that can be used for modelling the queue with a loop processing unit.
    [<CompiledName ("QueueLoopMerging")>]
    val queueLoopMerging: merge:(Stream<'a> -> Stream<'d> -> Stream<'e>)
                            -> enqueue:('e -> Proc<unit>)
                            -> dequeue:Proc<'c>
                            -> cond:Processor<'c, Choice<'f, 'b>>
                            -> body:Processor<'f, 'd>
                            -> Processor<'a, 'b>

    /// Returns a new processor that can be used for modelling the queue with a loop processing unit, where the input streams are merged sequentially.
    [<CompiledName ("QueueLoopSeq")>]
    val queueLoopSeq: enqueue:('a -> Proc<unit>)
                        -> dequeue:Proc<'c>
                        -> cond:Processor<'c, Choice<'e, 'b>>
                        -> body:Processor<'e, 'a>
                        -> Processor<'a, 'b>

    /// Returns a new processor that can be used for modelling the queue with a loop processing unit, where the input streams are merged simultaneously.
    [<CompiledName ("QueueLoopPar")>]
    val queueLoopPar: enqueue:('a -> Proc<unit>)
                        -> dequeue:Proc<'c>
                        -> cond:Processor<'c, Choice<'e, 'b>>
                        -> body:Processor<'e, 'a>
                        -> Processor<'a, 'b>

    /// Converts the specified signal transform to a processor.
    [<CompiledName ("OfSignaling")>]
    val ofSignaling: f:(Signal<'a> -> #Signal<'b>) -> Processor<'a, 'b>

    /// Convert the specified processor to a signal transform.
    [<CompiledName ("ToSignaling")>]
    val toSignaling: comp:Processor<'a, 'b> -> signal:Signal<'a> -> Proc<Signal<'b>>
    
    /// Returns a computation that holds the process for a random time interval distributed uniformly with the specified minimum and maximum, when processing every input element.
    [<CompiledName ("RandomUniform")>]
    val randomUniform: minimum:float -> maximum:float -> Processor<'a, 'a>
    
    /// Returns a computation that holds the process for an integer random time interval distributed uniformly with the specified minimum and maximum, when processing every input element.
    [<CompiledName ("RandomUniformInt")>]
    val randomUniformInt: minimum:int -> maximum:int -> Processor<'a, 'a>

    /// Returns a computation that holds the process for a triangular random time interval with the specified minimum, median and maximum, when processing every input element.
    [<CompiledName ("RandomTriangular")>]
    val randomTriangular: minimum:float -> median:float -> maximum:float -> Processor<'a, 'a>
    
    /// Returns a computation that holds the process for a random time interval distributed normally with the specified mean and deviation, when processing every input element.
    [<CompiledName ("RandomNormal")>]
    val randomNormal: mean:float -> deviation:float -> Processor<'a, 'a>
    
    /// Returns a computation that holds the process for a random time interval distributed exponentially with the specified mean (the reciprocal of the rate), when processing every input element.
    [<CompiledName ("RandomExponential")>]
    val randomExponential: mean:float -> Processor<'a, 'a>
    
    /// Returns a computation that holds the process for a random time interval having the Erlang distribution with the specified scale (the reciprocal of the rate) and integer shape, when processing every input element.
    [<CompiledName ("RandomErlang")>]
    val randomErlang: beta:float -> m:int -> Processor<'a, 'a>
    
    /// Returns a computation that holds the process for a random time interval having the Poisson distribution with the specified mean, when processing every input element.
    [<CompiledName ("RandomPoisson")>]
    val randomPoisson: mean:float -> Processor<'a, 'a>
    
    /// Returns a computation that holds the process for a random time interval distributed binomially with the specified probability and number of trials, when processing every input element.
    [<CompiledName ("RandomBinomial")>]
    val randomBinomial: prob:float -> trials:int -> Processor<'a, 'a>
