
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

/// Represents a preemptible resource.
[<Sealed>]
type PreemptibleResource

/// This module contains functions for working with the preemptible resources.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module PreemptibleResource =

    /// Returns the maximum count of the resource defined at time of initializing the resource, where None means that the resource has no upper bound.
    [<CompiledName ("MaxCount")>]
    val maxCount: resource:PreemptibleResource -> int option 

    /// Returns a computation of the current count of the resource.
    [<CompiledName ("Count")>]
    val count: resource:PreemptibleResource -> Eventive<int>

    /// Returns a computation of the statistics for the available count of the resource.
    [<CompiledName ("CountStats")>]
    val countStats: resource:PreemptibleResource -> Eventive<TimingStats<int>>

    /// Returns a signal triggered when the current available count of the resource changes.
    [<CompiledName ("CountChanged")>]
    val countChanged: resource:PreemptibleResource -> Signal<int>

    /// Returns a signal triggered when the current available count of the resource changes.
    [<CompiledName ("CountChanged_")>]
    val countChanged_: resource:PreemptibleResource -> Signal<unit>

    /// Returns a computation of the current utilisation count of the resource.
    [<CompiledName ("UtilisationCount")>]
    val utilisationCount: resource:PreemptibleResource -> Eventive<int>

    /// Returns a computation of the statistics for the utilisation count of the resource.
    [<CompiledName ("UtilisationCountStats")>]
    val utilisationCountStats: resource:PreemptibleResource -> Eventive<TimingStats<int>>

    /// Returns a signal triggered when the current utilisation count of the resource changes.
    [<CompiledName ("UtilisationCountChanged")>]
    val utilisationCountChanged: resource:PreemptibleResource -> Signal<int>

    /// Returns a signal triggered when the current utilisation count of the resource changes.
    [<CompiledName ("UtilisationCountChanged_")>]
    val utilisationCountChanged_: resource:PreemptibleResource -> Signal<unit>

    /// Returns a computation of the current queue length of the resource.
    [<CompiledName ("QueueCount")>]
    val queueCount: resource:PreemptibleResource -> Eventive<int>

    /// Returns a computation of the statistics for the queue length of the resource.
    [<CompiledName ("QueueCountStats")>]
    val queueCountStats: resource:PreemptibleResource -> Eventive<TimingStats<int>>

    /// Returns a signal triggered when the current queue length of the resource changes.
    [<CompiledName ("QueueCountChanged")>]
    val queueCountChanged: resource:PreemptibleResource -> Signal<int>

    /// Returns a signal triggered when the current queue length of the resource changes.
    [<CompiledName ("QueueCountChanged_")>]
    val queueCountChanged_: resource:PreemptibleResource -> Signal<unit>

    /// Returns a computation of the total wait time of the resource.
    [<CompiledName ("TotalWaitTime")>]
    val totalWaitTime: resource:PreemptibleResource -> Eventive<float>

    /// Returns a computation of the statistics for the wait time of the resource.
    [<CompiledName ("WaitTime")>]
    val waitTime: resource:PreemptibleResource -> Eventive<SamplingStats<float>>

    /// Returns a signal triggered when the wait time of the resource changes.
    [<CompiledName ("WaitTimeChanged_")>]
    val waitTimeChanged_: resource:PreemptibleResource -> Signal<unit>

    /// Returns a signal triggered when the wait time of the resource changes.
    [<CompiledName ("WaitTimeChanged")>]
    val waitTimeChanged: resource:PreemptibleResource -> Signal<SamplingStats<float>>

    /// Returns a signal triggered when one of the resource counters changes.
    [<CompiledName ("Changed_")>]
    val changed_: resource:PreemptibleResource -> Signal<unit>

    /// Creates a new resource with the specified initial and maximum available counts.
    [<CompiledName ("CreateWithMaxCount")>]
    val createWithMaxCount: count:int -> maxCount:int option -> Eventive<PreemptibleResource> 

    /// Creates a new resource with the specified initial count, where the last value becomes the upper bound as well.
    [<CompiledName ("Create")>]
    val create: count:int -> Eventive<PreemptibleResource> 

    /// Requests with the priority for the resource decreasing its count in case of success, or preempting another process with lower priority (less value is higher); otherwise, suspends the discontinuous process until another activity releases the resource.
    [<CompiledName ("RequestWithPriority")>]
    val requestWithPriority: priority:Priority -> resource:PreemptibleResource -> Proc<unit>

    /// Releases the resource increasing its count or resuming one of the previously suspended / preempted processes as possible.
    [<CompiledName ("Release")>]
    val release: resource:PreemptibleResource -> Proc<unit>

    /// Acquires the resource with the specified priority and retuns an IDisposable that, being applied, will release the resource.
    [<CompiledName ("TakeWithPriority")>]
    val takeWithPriority: priority:Priority -> resource:PreemptibleResource -> Proc<IDisposable>

    /// Increases the count of available resource by the specified number, invoking the awaiting / preempted processes as needed.
    [<CompiledName ("IncCount")>]
    val incCount: n:int -> resource:PreemptibleResource -> Eventive<unit>

    /// Decreases the count of available resource by the specified number, preempting the processes with lower priorities (less value is higher) as needed.
    [<CompiledName ("DecCount")>]
    val decCount: n:int -> resource:PreemptibleResource -> Eventive<unit> 

    /// Either increases or decreases the count of available resource by the specified number, invoking the awaiting / preempted processes or preempting the processes with lower priorities (less value is higher) as needed.
    [<CompiledName ("AlterCount")>]
    val alterCount: n:int -> resource:PreemptibleResource -> Eventive<unit> 