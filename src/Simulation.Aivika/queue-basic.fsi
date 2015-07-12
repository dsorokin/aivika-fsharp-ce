
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

namespace Simulation.Aivika.Basic

open System

open Simulation.Aivika

/// Represents an optimised finite queue parameterised by the type of items stored in the queue.
[<Sealed>]
type Queue<'a>

/// The module contains useful functions for working with the finite queues.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Queue =

    /// Returns the queue capacity.
    [<CompiledName ("MaxCount")>]
    val maxCount: queue:Queue<'a> -> int

    /// Returns the strategy applied to the input (enqueueing) process.
    [<CompiledName ("InputStrategy")>]
    val inputStrategy: queue:Queue<'a> -> IQueueStrategy

    /// Returns the strategy applied when storing (in memory) items in the queue.
    [<CompiledName ("StoringStrategy")>]
    val storingStrategy: queue:Queue<'a> -> IQueueStrategy

    /// Returns the strategy applied to the output (dequeueing) process. 
    [<CompiledName ("OutputStrategy")>]
    val outputStrategy: queue:Queue<'a> -> IQueueStrategy

    /// Creates a new finite queue with the specified strategies and capacity.
    [<CompiledName ("Create")>]
    val create<'si, 'sm, 'so, 'a 
                    when 'si :> IQueueStrategy and
                         'sm :> IQueueStrategy and
                         'so :> IQueueStrategy> : 
                            inputStrat:'si 
                            -> storingStrat:'sm
                            -> outputStrat:'so
                            -> maxCount:int
                            -> Simulation<Queue<'a>>

    /// Creates a new finite FCFS queue with the specified capacity.
    [<CompiledName ("CreateUsingFCFS")>]
    val createUsingFCFS<'a> : maxCount:int -> Simulation<Queue<'a>>

    /// Creates a new finite LCFS queue with the specified capacity.
    [<CompiledName ("CreateUsingLCFS")>]
    val createUsingLCFS<'a> : maxCount:int -> Simulation<Queue<'a>>

    /// Creates a new finite SIRO queue with the specified capacity.
    [<CompiledName ("CreateUsingSIRO")>]
    val createUsingSIRO<'a> : maxCount:int -> Simulation<Queue<'a>>

    /// Creates a new finite priority queue with the specified capacity.
    [<CompiledName ("CreateUsingPriorities")>]
    val createUsingPriorities<'a> : maxCount:int -> Simulation<Queue<'a>>

    /// Dequeues suspending the process if the queue is empty.
    [<CompiledName ("Dequeue")>]
    val dequeue: queue:Queue<'a> -> Proc<'a>

    /// Dequeues with the specified priority used for output, where the process suspends if the queue is empty.
    [<CompiledName ("DequeueWithOutputPriority")>]
    val dequeueWithOutputPriority: po:Priority -> queue:Queue<'a> -> Proc<'a>
    
    /// Tries to dequeue immediately without suspension.
    [<CompiledName ("TryDequeue")>]
    val tryDequeue: queue:Queue<'a> -> Eventive<'a option>

    /// Tries to remove an element satisfying the specified predicate.
    [<CompiledName ("DeleteBy")>]
    val deleteBy: pred:('a -> bool) -> queue:Queue<'a> -> Eventive<'a option>

    /// Tries to remove an element satisfying the specified predicate.
    [<CompiledName ("DeleteBy_")>]
    val deleteBy_: pred:('a -> bool) -> queue:Queue<'a> -> Eventive<unit>

    /// Tries to remove the specified item from the queue.
    [<CompiledName ("Delete")>]
    val delete: item:'a -> queue:Queue<'a> -> Eventive<bool> when 'a : equality

    /// Tries to remove the specified item from the queue.
    [<CompiledName ("Delete_")>]
    val delete_: item:'a -> queue:Queue<'a> -> Eventive<unit> when 'a : equality

    /// Removes all elements from the queue.
    [<CompiledName ("Clear")>]
    val clear: queue:Queue<'a> -> Eventive<unit>

    /// Enqueues an item suspending the process if the queue is full.
    [<CompiledName ("Enqueue")>]
    val enqueue: item:'a -> queue:Queue<'a> -> Proc<unit>

    /// Enqueues with the input priority an item suspending the process if the queue is full.
    [<CompiledName ("EnqueueWithInputPriority")>]
    val enqueueWithInputPriority: pi:Priority -> item:'a -> queue:Queue<'a> -> Proc<unit>

    /// Enqueues with the storing priority an item suspending the process if the queue is full.
    [<CompiledName ("EnqueueWithStoringPriority")>]
    val enqueueWithStoringPriority: pm:Priority -> item:'a -> queue:Queue<'a> -> Proc<unit>

    /// Enqueues with the input and storing priorities an item suspending the process if the queue is full.
    [<CompiledName ("EnqueueWithInputStoringPriorities")>]
    val enqueueWithInputStoringPriorities: pi:Priority -> pm:Priority -> item:'a -> queue:Queue<'a> -> Proc<unit>

    /// Tries to enqueue an item immediately without suspension and returns a boolean flag indicating whether the operation was successful, which might fail if the queue was full.
    [<CompiledName ("TryEnqueue")>]
    val tryEnqueue: item:'a -> queue:Queue<'a> -> Eventive<bool>

    /// Tries to enqueue with the storing priority an item immediately without suspension and returns a boolean flag indicating whether the operation was successful, which might fail if the queue was full.
    [<CompiledName ("TryEnqueueWithStoringPriority")>]
    val tryEnqueueWithStoringPriority: pm:Priority -> item:'a -> queue:Queue<'a> -> Eventive<bool>

    /// Tests whether the queue is empty.
    [<CompiledName ("IsEmpty")>]
    val isEmpty: queue:Queue<'a> -> Eventive<bool>
    
    /// Tests whether the queue is full.
    [<CompiledName ("IsFull")>]
    val isFull: queue:Queue<'a> -> Eventive<bool>

    /// Returns the current queue size.
    [<CompiledName ("Count")>]
    val count: queue:Queue<'a> -> Eventive<int>
