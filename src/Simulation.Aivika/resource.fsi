
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

/// Represents a limited resource parameterised by the queue strategy.
[<Sealed>]
type Resource

/// This module contains functions for working with the resources.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Resource =

    /// Returns the strategy applied for queueing the concurrent requests.
    [<CompiledName ("Strategy")>]
    val strategy: resource:Resource -> IQueueStrategy

    /// Returns the maximum count of the resource defined at time of initializing the resource, where None means that the resource has no upper bound.
    [<CompiledName ("MaxCount")>]
    val maxCount: resource:Resource -> int option 

    /// Returns a computation of the current available count of the resource.
    [<CompiledName ("Count")>]
    val count: resource:Resource -> Eventive<int>

    /// Returns a computation of the statistics for the available count of the resource.
    [<CompiledName ("CountStats")>]
    val countStats: resource:Resource -> Eventive<TimingStats<int>>

    /// Returns a signal triggered when the current available count of the resource changes.
    [<CompiledName ("CountChanged")>]
    val countChanged: resource:Resource -> Signal<int>

    /// Returns a signal triggered when the current available count of the resource changes.
    [<CompiledName ("CountChanged_")>]
    val countChanged_: resource:Resource -> Signal<unit>

    /// Returns a computation of the current utilisation count of the resource.
    [<CompiledName ("UtilisationCount")>]
    val utilisationCount: resource:Resource -> Eventive<int>

    /// Returns a computation of the statistics for the utilisation count of the resource.
    [<CompiledName ("UtilisationCountStats")>]
    val utilisationCountStats: resource:Resource -> Eventive<TimingStats<int>>

    /// Returns a signal triggered when the current utilisation count of the resource changes.
    [<CompiledName ("UtilisationCountChanged")>]
    val utilisationCountChanged: resource:Resource -> Signal<int>

    /// Returns a signal triggered when the current utilisation count of the resource changes.
    [<CompiledName ("UtilisationCountChanged_")>]
    val utilisationCountChanged_: resource:Resource -> Signal<unit>

    /// Returns a computation of the current queue length of the resource.
    [<CompiledName ("QueueCount")>]
    val queueCount: resource:Resource -> Eventive<int>

    /// Returns a computation of the statistics for the queue length of the resource.
    [<CompiledName ("QueueCountStats")>]
    val queueCountStats: resource:Resource -> Eventive<TimingStats<int>>

    /// Returns a signal triggered when the current queue length of the resource changes.
    [<CompiledName ("QueueCountChanged")>]
    val queueCountChanged: resource:Resource -> Signal<int>

    /// Returns a signal triggered when the current queue length of the resource changes.
    [<CompiledName ("QueueCountChanged_")>]
    val queueCountChanged_: resource:Resource -> Signal<unit>

    /// Returns a computation of the total wait time of the resource.
    [<CompiledName ("TotalWaitTime")>]
    val totalWaitTime: resource:Resource -> Eventive<float>

    /// Returns a computation of the statistics for the wait time of the resource.
    [<CompiledName ("WaitTime")>]
    val waitTime: resource:Resource -> Eventive<SamplingStats<float>>

    /// Returns a signal triggered when the wait time of the resource changes.
    [<CompiledName ("WaitTimeChanged_")>]
    val waitTimeChanged_: resource:Resource -> Signal<unit>

    /// Returns a signal triggered when the wait time of the resource changes.
    [<CompiledName ("WaitTimeChanged")>]
    val waitTimeChanged: resource:Resource -> Signal<SamplingStats<float>>

    /// Returns a signal triggered when one of the resource counters changes.
    [<CompiledName ("Changed_")>]
    val changed_: resource:Resource -> Signal<unit>

    /// Creates a new resource with the specified queue strategy, initial and maximum available counts.
    [<CompiledName ("CreateWithMaxCount")>]
    val createWithMaxCount: strat:#IQueueStrategy -> count:int -> maxCount:int option -> Eventive<Resource> 

    /// Creates a new resource with the specified queue strategy and initial count, where the last value becomes the upper bound as well.
    [<CompiledName ("Create")>]
    val create: strat:#IQueueStrategy -> count:int -> Eventive<Resource> 

    /// Creates a new resource with the specified initial count applying strategy FCFS, where the initial count becomes the upper bound as well.
    [<CompiledName ("CreateUsingFCFS")>]
    val createUsingFCFS: count:int -> Eventive<Resource>

    /// Creates a new resource with the specified initial and maximum available counts applying strategy FCFS.
    [<CompiledName ("CreateWithMaxCountUsingFCFS")>]
    val createWithMaxCountUsingFCFS: count:int -> maxCount:int option -> Eventive<Resource>

    /// Creates a new resource with the specified initial count applying strategy LCFS, where the initial count becomes the upper bound as well.
    [<CompiledName ("CreateUsingLCFS")>]
    val createUsingLCFS: count:int -> Eventive<Resource>

    /// Creates a new resource with the specified initial and maximum available counts applying strategy LCFS.
    [<CompiledName ("CreateWithMaxCountUsingLCFS")>]
    val createWithMaxCountUsingLCFS: count:int -> maxCount:int option -> Eventive<Resource>

    /// Creates a new resource with the specified initial count applying strategy SIRO, where the initial count becomes the upper bound as well.
    [<CompiledName ("CreateUsingSIRO")>]
    val createUsingSIRO: count:int -> Eventive<Resource>

    /// Creates a new resource with the specified initial and maximum available counts applying strategy SIRO.
    [<CompiledName ("CreateWithMaxCountUsingSIRO")>]
    val createWithMaxCountUsingSIRO: count:int -> maxCount:int option -> Eventive<Resource>

    /// Creates a new resource with the specified initial count using static priorities, where the initial count becomes the upper bound as well.
    [<CompiledName ("CreateUsingPriorities")>]
    val createUsingPriorities: count:int -> Eventive<Resource>

    /// Creates a new resource with the specified initial and maximum available counts using static priorities.
    [<CompiledName ("CreateWithMaxCountUsingPriorities")>]
    val createWithMaxCountUsingPriorities: count:int -> maxCount:int option -> Eventive<Resource>

    /// Requests for the resource decreasing its count in case of success; otherwise, suspends the discontinuous process until another activity releases the resource.
    [<CompiledName ("Request")>]
    val request: resource:Resource -> Proc<unit>

    /// Requests with the priority for the resource decreasing its count in case of success; otherwise, suspends the discontinuous process until another activity releases the resource.
    [<CompiledName ("RequestWithPriority")>]
    val requestWithPriority: priority:Priority -> resource:Resource -> Proc<unit>

    /// Releases the resource increasing its count and resuming one of the previously suspended processes as possible.
    [<CompiledName ("ReleaseWithinEventive")>]
    val releaseWithinEventive: resource:Resource -> Eventive<unit>

    /// Releases the resource increasing its count and resuming one of the previously suspended processes as possible (the function has a more convenient type signature).
    [<CompiledName ("Release")>]
    val release: resource:Resource -> Proc<unit>

    /// Tries to request for the resource decreasing its count in case of success and returning true within the computation; otherwise, returning false.
    [<CompiledName ("TryRequestWithinEventive")>]
    val tryRequestWithinEventive: resource:Resource -> Eventive<bool>

    /// Acquires the resource and retuns an IDisposable that, being applied, will release the resource.
    [<CompiledName ("Take")>]
    val take: resource:Resource -> Proc<IDisposable>

    /// Acquires the resource with the specified priority and retuns an IDisposable that, being applied, will release the resource.
    [<CompiledName ("TakeWithPriority")>]
    val takeWithPriority: priority:Priority -> resource:Resource -> Proc<IDisposable>

    /// Increases the count of available resource by the specified number, invoking the awaiting processes as needed.
    [<CompiledName ("IncCount")>]
    val incCount: n:int -> resource:Resource -> Eventive<unit> 

    /// Decrease the count of available resource by the specified number, waiting for the processes that captured the resource if needed.
    [<CompiledName ("DecCount")>]
    val decCount: n:int -> resource:Resource -> Proc<unit> 
