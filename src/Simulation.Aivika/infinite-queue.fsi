
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

/// Represents an infinite queue parameterised by the type of items stored in the queue.
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
                            -> Eventive<InfiniteQueue<'a>>

    /// Creates a new infinite FCFS queue.
    [<CompiledName ("CreateUsingFCFS")>]
    val createUsingFCFS<'a> : Eventive<InfiniteQueue<'a>>

    /// Creates a new infinite LCFS queue.
    [<CompiledName ("CreateUsingLCFS")>]
    val createUsingLCFS<'a> : Eventive<InfiniteQueue<'a>>

    /// Creates a new infinite SIRO queue.
    [<CompiledName ("CreateUsingSIRO")>]
    val createUsingSIRO<'a> : Eventive<InfiniteQueue<'a>>

    /// Creates a new infinite queue using priorities.
    [<CompiledName ("CreateUsingPriorities")>]
    val createUsingPriorities<'a> : Eventive<InfiniteQueue<'a>>

    /// Returns a signal that notifies when the enqueued item is stored in the internal memory of the queue.
    [<CompiledName ("EnqueueStored")>]
    val enqueueStored: queue:InfiniteQueue<'a> -> Signal<'a>

    /// Returns a signal that notifies when the dequeueing operation was requested.
    [<CompiledName ("DequeueRequested")>]
    val dequeueRequested: queue:InfiniteQueue<'a> -> Signal<unit>

    /// Returns a signal that notifies when the item was extracted from the internal storage of the queue and prepared for immediate receiving by the dequeueing process.
    [<CompiledName ("DequeueExtracted")>]
    val dequeueExtracted: queue:InfiniteQueue<'a> -> Signal<'a>

    /// Dequeues and item suspending the process if the queue is empty.
    [<CompiledName ("Dequeue")>]
    val dequeue: queue:InfiniteQueue<'a> -> Proc<'a>

    /// Dequeues an item using the specified output priority, when the process suspends if the queue is empty.
    [<CompiledName ("DequeueWithOutputPriority")>]
    val dequeueWithOutputPriority: po:Priority -> queue:InfiniteQueue<'a> -> Proc<'a>
    
    /// Tries to dequeue an item immediately without suspension.
    [<CompiledName ("TryDequeue")>]
    val tryDequeue: queue:InfiniteQueue<'a> -> Eventive<'a option>

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

    /// Returns the queue size statistics.
    [<CompiledName ("CountStats")>]
    val countStats: queue:InfiniteQueue<'a> -> Eventive<TimingStats<int>>

    /// Returns a signal triggered when the current queue size changes.
    [<CompiledName ("CountChanged_")>]
    val countChanged_: queue:InfiniteQueue<'a> -> Signal<unit>

    /// Returns a signal triggered when the current queue size changes.
    [<CompiledName ("CountChanged")>]
    val countChanged: queue:InfiniteQueue<'a> -> Signal<int>

    /// Returns the total number of input items that were stored.
    [<CompiledName ("StoreCount")>]
    val storeCount: queue:InfiniteQueue<'a> -> Eventive<int>

    /// Returns a signal triggered when changing the total number of input items that were stored.
    [<CompiledName ("StoreCountChanged_")>]
    val storeCountChanged_: queue:InfiniteQueue<'a> -> Signal<unit>

    /// Returns a signal triggered when changing the total number of input items that were stored.
    [<CompiledName ("StoreCountChanged")>]
    val storeCountChanged: queue:InfiniteQueue<'a> -> Signal<int>

    /// Returns the total number of requests to dequeue the items, not counting attempts to dequeue immediately without suspension.
    [<CompiledName ("OutputRequestCount")>]
    val outputRequestCount: queue:InfiniteQueue<'a> -> Eventive<int>

    /// Returns a signal triggered when changing the total number of requests to dequeue the items.
    [<CompiledName ("OutputRequestCountChanged_")>]
    val outputRequestCountChanged_: queue:InfiniteQueue<'a> -> Signal<unit>

    /// Returns a signal triggered when changing the total number of requests to dequeue the items.
    [<CompiledName ("OutputRequestCountChanged")>]
    val outputRequestCountChanged: queue:InfiniteQueue<'a> -> Signal<int>

    /// Returns the total number of output items that were dequeued.
    [<CompiledName ("OutputCount")>]
    val outputCount: queue:InfiniteQueue<'a> -> Eventive<int>

    /// Returns a signal triggered when changing the total number of output items that were dequeued.
    [<CompiledName ("OutputCountChanged_")>]
    val outputCountChanged_: queue:InfiniteQueue<'a> -> Signal<unit>

    /// Returns a signal triggered when changing the total number of output items that were dequeued.
    [<CompiledName ("OutputCountChanged")>]
    val outputCountChanged: queue:InfiniteQueue<'a> -> Signal<int>

    /// Returns the rate of the items that were stored: how many items per time.
    [<CompiledName ("StoreRate")>]
    val storeRate: queue:InfiniteQueue<'a> -> Eventive<float>

    /// Returns the rate of requests for dequeueing the items: how many requests per time, not counting attempts to dequeue immediately without suspension.
    [<CompiledName ("OutputRequestRate")>]
    val outputRequestRate: queue:InfiniteQueue<'a> -> Eventive<float>

    /// Returns the rate of the items that were dequeued: how many items per time.
    [<CompiledName ("OutputRate")>]
    val outputRate: queue:InfiniteQueue<'a> -> Eventive<float>

    /// Returns the wait time from the time at which the item was stored in the queue to the time at which it was dequeued.
    [<CompiledName ("WaitTime")>]
    val waitTime: queue:InfiniteQueue<'a> -> Eventive<SamplingStats<Time>>

    /// Returns a signal triggered when changing the wait time from the time at which the item was stored in the queue to the time at which it was dequeued.
    [<CompiledName ("WaitTimeChanged_")>]
    val waitTimeChanged_: queue:InfiniteQueue<'a> -> Signal<unit>

    /// Returns a signal triggered when changing the wait time from the time at which the item was stored in the queue to the time at which it was dequeued.
    [<CompiledName ("WaitTimeChanged")>]
    val waitTimeChanged: queue:InfiniteQueue<'a> -> Signal<SamplingStats<Time>>

    /// Returns the output wait time from the time at which the item was requested for dequeueing to the time at which it was actually dequeued.
    [<CompiledName ("OutputWaitTime")>]
    val outputWaitTime: queue:InfiniteQueue<'a> -> Eventive<SamplingStats<Time>>

    /// Returns a signal triggered when changing the output wait time from the time at which the item was requested for dequeueing to the time at which it was actually dequeued.
    [<CompiledName ("OutputWaitTimeChanged_")>]
    val outputWaitTimeChanged_: queue:InfiniteQueue<'a> -> Signal<unit>

    /// Returns a signal triggered when changing the output wait time from the time at which the item was requested for dequeueing to the time at which it was actually dequeued.
    [<CompiledName ("OutputWaitTimeChanged")>]
    val outputWaitTimeChanged: queue:InfiniteQueue<'a> -> Signal<SamplingStats<Time>>

    /// Returns a long-term average queue rate calculated as the average queue size divided by the average wait time.
    [<CompiledName ("Rate")>]
    val rate: queue:InfiniteQueue<'a> -> Eventive<float>

    /// Returns a signal triggered when changing the rate.
    [<CompiledName ("RateChanged_")>]
    val rateChanged_: queue:InfiniteQueue<'a> -> Signal<unit>

    /// Returns a signal triggered when changing the rate.
    [<CompiledName ("RateChanged")>]
    val rateChanged: queue:InfiniteQueue<'a> -> Signal<float>

    /// Returns a signal triggered when changing the queue's state.
    [<CompiledName ("Changed_")>]
    val changed_: queue:InfiniteQueue<'a> -> Signal<unit>
