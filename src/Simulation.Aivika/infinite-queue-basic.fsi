
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

/// Represents an optimised infinite queue parameterised by the type of items stored in the queue.
[<Sealed>]
type InfiniteQueue<'a>

/// The module contains useful functions for working with the infinite queues.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module InfiniteQueue =

    /// Returns the strategy applied when storing (in memory) items in the queue.
    [<CompiledName ("StoringStrategy")>]
    val storingStrategy: queue:InfiniteQueue<'a> -> IQueueStrategy

    /// Returns the strategy applied to the output (dequeueing) process.
    [<CompiledName ("OutputStrategy")>]
    val outputStrategy: queue:InfiniteQueue<'a> -> IQueueStrategy

    /// Creates a new infinite queue with the specified strategies.
    [<CompiledName ("Create")>]
    val create<'sm, 'so, 'a 
                    when 'sm :> IQueueStrategy and
                         'so :> IQueueStrategy> : 
                            storingStrat:'sm
                            -> outputStrat:'so
                            -> Simulation<InfiniteQueue<'a>>

    /// Creates a new infinite FCFS queue.
    [<CompiledName ("CreateUsingFCFS")>]
    val createUsingFCFS<'a> : Simulation<InfiniteQueue<'a>>

    /// Creates a new infinite LCFS queue.
    [<CompiledName ("CreateUsingLCFS")>]
    val createUsingLCFS<'a> : Simulation<InfiniteQueue<'a>>

    /// Creates a new infinite SIRO queue.
    [<CompiledName ("CreateUsingSIRO")>]
    val createUsingSIRO<'a> : Simulation<InfiniteQueue<'a>>

    /// Creates a new infinite queue using priorities.
    [<CompiledName ("CreateUsingPriorities")>]
    val createUsingPriorities<'a> : Simulation<InfiniteQueue<'a>>

    /// Dequeues and item suspending the process if the queue is empty.
    [<CompiledName ("Dequeue")>]
    val dequeue: queue:InfiniteQueue<'a> -> Proc<'a>

    /// Dequeues an item using the specified output priority, when the process suspends if the queue is empty.
    [<CompiledName ("DequeueWithOutputPriority")>]
    val dequeueWithOutputPriority: po:Priority -> queue:InfiniteQueue<'a> -> Proc<'a>
    
    /// Tries to dequeue an item immediately without suspension.
    [<CompiledName ("TryDequeue")>]
    val tryDequeue: queue:InfiniteQueue<'a> -> Eventive<'a option>

    /// Tries to remove an element satisfying the specified predicate.
    [<CompiledName ("DeleteBy")>]
    val deleteBy: pred:('a -> bool) -> queue:InfiniteQueue<'a> -> Eventive<'a option>

    /// Tries to remove an element satisfying the specified predicate.
    [<CompiledName ("DeleteBy_")>]
    val deleteBy_: pred:('a -> bool) -> queue:InfiniteQueue<'a> -> Eventive<unit>

    /// Tries to remove the specified item from the queue.
    [<CompiledName ("Delete")>]
    val delete: item:'a -> queue:InfiniteQueue<'a> -> Eventive<bool> when 'a : equality

    /// Tries to remove the specified item from the queue.
    [<CompiledName ("Delete_")>]
    val delete_: item:'a -> queue:InfiniteQueue<'a> -> Eventive<unit> when 'a : equality

    /// Removes all elements from the queue.
    [<CompiledName ("Clear")>]
    val clear: queue:InfiniteQueue<'a> -> Eventive<unit>

    /// Enqueues the item immediately without suspension.
    [<CompiledName ("Enqueue")>]
    val enqueue: item:'a -> queue:InfiniteQueue<'a> -> Eventive<unit>

    /// Enqueues with the storing priority an item immediately without suspension.
    [<CompiledName ("EnqueueWithStoringPriority")>]
    val enqueueWithStoringPriority: pm:Priority -> item:'a -> queue:InfiniteQueue<'a> -> Eventive<unit>
    
    /// Tests whether the queue is empty.
    [<CompiledName ("IsEmpty")>]
    val isEmpty: queue:InfiniteQueue<'a> -> Eventive<bool>

    /// Returns the current queue size.
    [<CompiledName ("Count")>]
    val count: queue:InfiniteQueue<'a> -> Eventive<int>
