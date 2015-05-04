
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

/// Represents a finite queue parameterised by the type of items stored in the queue.
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
                            -> Eventive<Queue<'a>>

    /// Creates a new finite FCFS queue with the specified capacity.
    [<CompiledName ("CreateUsingFCFS")>]
    val createUsingFCFS<'a> : maxCount:int -> Eventive<Queue<'a>>

    /// Creates a new finite LCFS queue with the specified capacity.
    [<CompiledName ("CreateUsingLCFS")>]
    val createUsingLCFS<'a> : maxCount:int -> Eventive<Queue<'a>>

    /// Creates a new finite SIRO queue with the specified capacity.
    [<CompiledName ("CreateUsingSIRO")>]
    val createUsingSIRO<'a> : maxCount:int -> Eventive<Queue<'a>>

    /// Creates a new finite priority queue with the specified capacity.
    [<CompiledName ("CreateUsingPriorities")>]
    val createUsingPriorities<'a> : maxCount:int -> Eventive<Queue<'a>>

    /// Returns a signal that notifies when the enqueueing operation is initiated.
    [<CompiledName ("EnqueueInitiated")>]
    val enqueueInitiated: queue:Queue<'a> -> Signal<'a>

    /// Returns a signal that notifies when the enqueueing operation is completed and the item is stored in the internal memory of the queue.
    [<CompiledName ("EnqueueStored")>]
    val enqueueStored: queue:Queue<'a> -> Signal<'a>

    /// Returns a signal that notifies when the enqueueing operation failed and the item was lost when trying to enqueue with help of functions that imply that the item can indeed be lost.
    [<CompiledName ("EnqueueLost")>]
    val enqueueLost: queue:Queue<'a> -> Signal<'a>

    /// Returns a signal that notifies when the dequeueing operation was requested.
    [<CompiledName ("DequeueRequested")>]
    val dequeueRequested: queue:Queue<'a> -> Signal<unit>

    /// Returns a signal that notifies when the item was extracted from the internal storage of the queue and prepared for immediate receiving by the dequeueing process.
    [<CompiledName ("DequeueExtracted")>]
    val dequeueExtracted: queue:Queue<'a> -> Signal<'a>

    /// Dequeues suspending the process if the queue is empty.
    [<CompiledName ("Dequeue")>]
    val dequeue: queue:Queue<'a> -> Proc<'a>

    /// Dequeues with the specified priority used for output, where the process suspends if the queue is empty.
    [<CompiledName ("DequeueWithOutputPriority")>]
    val dequeueWithOutputPriority: po:Priority -> queue:Queue<'a> -> Proc<'a>
    
    /// Tries to dequeue immediately without suspension.
    [<CompiledName ("TryDequeue")>]
    val tryDequeue: queue:Queue<'a> -> Eventive<'a option>

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

    /// Tries to enqueue an item immediately and then returns true; otherwise, if the queue is full then the item is lost and false is returned.
    [<CompiledName ("EnqueueOrLost")>]
    val enqueueOrLost: item:'a -> queue:Queue<'a> -> Eventive<bool>

    /// Tries to enqueue with the storing priority an item immediately and then returns true; otherwise, if the queue is full then the item is lost and false is returned.
    [<CompiledName ("EnqueueWithStoringPriorityOrLost")>]
    val enqueueWithStoringPriorityOrLost: pm:Priority -> item:'a -> queue:Queue<'a> -> Eventive<bool>

    /// Tries to enqueue an item immediately; otherwise, if the queue is full then the item is lost.
    [<CompiledName ("EnqueueOrLost_")>]
    val enqueueOrLost_: item:'a -> queue:Queue<'a> -> Eventive<unit>

    /// Tries to enqueue with the storing priority an item immediately; otherwise, if the queue is full then the item is lost.
    [<CompiledName ("EnqueueWithStoringPriorityOrLost_")>]
    val enqueueWithStoringPriorityOrLost_: pm:Priority -> item:'a -> queue:Queue<'a> -> Eventive<unit>
    
    /// Tests whether the queue is empty.
    [<CompiledName ("IsEmpty")>]
    val isEmpty: queue:Queue<'a> -> Eventive<bool>
    
    /// Tests whether the queue is full.
    [<CompiledName ("IsFull")>]
    val isFull: queue:Queue<'a> -> Eventive<bool>

    /// Returns the current queue size.
    [<CompiledName ("Count")>]
    val count: queue:Queue<'a> -> Eventive<int>

    /// Returns the queue size statistics.
    [<CompiledName ("CountStats")>]
    val countStats: queue:Queue<'a> -> Eventive<TimingStats<int>>

    /// Returns a signal triggered when the queue size changes.
    [<CompiledName ("CountChanged_")>]
    val countChanged_: queue:Queue<'a> -> Signal<unit>

    /// Returns a signal triggered when the queue size changes.
    [<CompiledName ("CountChanged")>]
    val countChanged: queue:Queue<'a> -> Signal<int>

    /// Returns the total number of lost items.
    [<CompiledName ("LostCount")>]
    val lostCount: queue:Queue<'a> -> Eventive<int>

    /// Returns a signal triggered when the total number of lost items changes.
    [<CompiledName ("LostCountChanged_")>]
    val lostCountChanged_: queue:Queue<'a> -> Signal<unit>

    /// Returns a signal triggered when the total number of lost items changes.
    [<CompiledName ("LostCountChanged")>]
    val lostCountChanged: queue:Queue<'a> -> Signal<int>

    /// Returns the total number of items that were enqueued.
    [<CompiledName ("InputCount")>]
    val inputCount: queue:Queue<'a> -> Eventive<int>

    /// Returns a signal triggered when changing the total number of items that were enqueued.
    [<CompiledName ("InputCountChanged_")>]
    val inputCountChanged_: queue:Queue<'a> -> Signal<unit>

    /// Returns a signal triggered when changing the total number of items that were enqueued.
    [<CompiledName ("InputCountChanged")>]
    val inputCountChanged: queue:Queue<'a> -> Signal<int>

    /// Returns the total number of input items that were stored.
    [<CompiledName ("StoreCount")>]
    val storeCount: queue:Queue<'a> -> Eventive<int>

    /// Returns a signal triggered when changing the total number of input items that were stored.
    [<CompiledName ("StoreCountChanged_")>]
    val storeCountChanged_: queue:Queue<'a> -> Signal<unit>

    /// Returns a signal triggered when changing the total number of input items that were stored.
    [<CompiledName ("StoreCountChanged")>]
    val storeCountChanged: queue:Queue<'a> -> Signal<int>

    /// Returns the total number of requests to dequeue the items, not counting attempts to dequeue immediately without suspension.
    [<CompiledName ("OutputRequestCount")>]
    val outputRequestCount: queue:Queue<'a> -> Eventive<int>

    /// Returns a signal triggered when changing the total number of requests to dequeue the items.
    [<CompiledName ("OutputRequestCountChanged_")>]
    val outputRequestCountChanged_: queue:Queue<'a> -> Signal<unit>

    /// Returns a signal triggered when changing the total number of requests to dequeue the items.
    [<CompiledName ("OutputRequestCountChanged")>]
    val outputRequestCountChanged: queue:Queue<'a> -> Signal<int>

    /// Returns the total number of output items that were dequeued.
    [<CompiledName ("OutputCount")>]
    val outputCount: queue:Queue<'a> -> Eventive<int>

    /// Returns a signal triggered when changing the total number of output items that were dequeued.
    [<CompiledName ("OutputCountChanged_")>]
    val outputCountChanged_: queue:Queue<'a> -> Signal<unit>

    /// Returns a signal triggered when changing the total number of output items that were dequeued.
    [<CompiledName ("OutputCountChanged")>]
    val outputCountChanged: queue:Queue<'a> -> Signal<int>

    /// Returns the load factor: the queue size divided by its capacity.
    [<CompiledName ("LoadFactor")>]
    val loadFactor: queue:Queue<'a> -> Eventive<float>

    /// Returns a signal triggered when changing the load factor.
    [<CompiledName ("LoadFactorChanged_")>]
    val loadFactorChanged_: queue:Queue<'a> -> Signal<unit>

    /// Returns a signal triggered when changing the load factor.
    [<CompiledName ("LoadFactorChanged")>]
    val loadFactorChanged: queue:Queue<'a> -> Signal<float>

    /// Returns the rate for the input items that were enqueued: how many items per time.
    [<CompiledName ("InputRate")>]
    val inputRate: queue:Queue<'a> -> Eventive<float>

    /// Returns the rate for the items that were stored: how many items per time.
    [<CompiledName ("StoreRate")>]
    val storeRate: queue:Queue<'a> -> Eventive<float>

    /// Returns the rate of the requests for dequeueing the items: how many requests per time, not counting attempts to dequeue immediately without suspension.
    [<CompiledName ("OutputRequestRate")>]
    val outputRequestRate: queue:Queue<'a> -> Eventive<float>

    /// Returns the rate of the items that were dequeued: how many items per time.
    [<CompiledName ("OutputRate")>]
    val outputRate: queue:Queue<'a> -> Eventive<float>

    /// Returns the wait time from the time at which the item was stored in the queue to the time at which it was dequeued.
    [<CompiledName ("WaitTime")>]
    val waitTime: queue:Queue<'a> -> Eventive<SamplingStats<Time>>

    /// Returns a signal triggered when changing the wait time.
    [<CompiledName ("WaitTimeChanged_")>]
    val waitTimeChanged_: queue:Queue<'a> -> Signal<unit>

    /// Returns a signal triggered when changing the wait time.
    [<CompiledName ("WaitTimeChanged")>]
    val waitTimeChanged: queue:Queue<'a> -> Signal<SamplingStats<Time>>

    /// Returns the total wait time from the time at which the enqueueing operation was initiated to the time at which the item was dequeued.
    [<CompiledName ("TotalWaitTime")>]
    val totalWaitTime: queue:Queue<'a> -> Eventive<SamplingStats<Time>>

    /// Returns a signal triggered when changing the total wait time.
    [<CompiledName ("TotalWaitTimeChanged_")>]
    val totalWaitTimeChanged_: queue:Queue<'a> -> Signal<unit>

    /// Returns a signal triggered when changing the total wait time.
    [<CompiledName ("TotalWaitTimeChanged")>]
    val totalWaitTimeChanged: queue:Queue<'a> -> Signal<SamplingStats<Time>>

    /// Returns the input wait time from the time at which the enqueueing operation was initiated to the time at which the item was stored in the queue.
    [<CompiledName ("InputWaitTime")>]
    val inputWaitTime: queue:Queue<'a> -> Eventive<SamplingStats<Time>>

    /// Returns a signal triggered when changing the input wait time.
    [<CompiledName ("InputWaitTimeChanged_")>]
    val inputWaitTimeChanged_: queue:Queue<'a> -> Signal<unit>

    /// Returns a signal triggered when changing the input wait time.
    [<CompiledName ("InputWaitTimeChanged")>]
    val inputWaitTimeChanged: queue:Queue<'a> -> Signal<SamplingStats<Time>>

    /// Returns the output wait time from the time at which the item was requested for dequeueing to the time at which it was actually dequeued.
    [<CompiledName ("OutputWaitTime")>]
    val outputWaitTime: queue:Queue<'a> -> Eventive<SamplingStats<Time>>

    /// Returns a signal triggered when changing the output wait time.
    [<CompiledName ("OutputWaitTimeChanged_")>]
    val outputWaitTimeChanged_: queue:Queue<'a> -> Signal<unit>

    /// Returns a signal triggered when changing the output wait time.
    [<CompiledName ("OutputWaitTimeChanged")>]
    val outputWaitTimeChanged: queue:Queue<'a> -> Signal<SamplingStats<Time>>

    /// Returns a long-term average queue rate calculated as the average queue size divided by the average wait time, which may differ from the arrival rate for the finite queue.
    [<CompiledName ("Rate")>]
    val rate: queue:Queue<'a> -> Eventive<float>

    /// Returns a signal triggered when changing the rate.
    [<CompiledName ("RateChanged_")>]
    val rateChanged_: queue:Queue<'a> -> Signal<unit>

    /// Returns a signal triggered when changing the rate.
    [<CompiledName ("RateChanged")>]
    val rateChanged: queue:Queue<'a> -> Signal<float>

    /// Returns a signal triggered when changing the queue's state.
    [<CompiledName ("Changed_")>]
    val changed_: queue:Queue<'a> -> Signal<unit>

    /// Returns a computation that waits while the queue is full.
    [<CompiledName ("WaitWhileFull")>]
    val waitWhileFull: queue:Queue<'a> -> Proc<unit>
