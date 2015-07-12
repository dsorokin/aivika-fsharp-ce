
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

/// The type abbreviation for the priority.
type Priority = float

/// Represents a queue storage.
[<Interface>]
type IQueueStorage<'a> =

    /// Tests whether the queue is empty.
    abstract IsEmpty: unit -> Eventive<bool>

    /// Dequeues the front element and returns it within the computation, or raises an exception if the operation is not supported.
    abstract Dequeue: unit -> Eventive<'a>

    /// Enqueues an element within the computation, or raises an exception if the operation is not supported.
    abstract Enqueue: item:'a -> Eventive<unit>

    /// Enqueues an element with the specified priority within the computation, or raises an exception if the operation is not supported.
    abstract Enqueue: priority:Priority * item:'a -> Eventive<unit>

    /// Removes an element satisfying the specified predicate, or raises an exception if the operation is not supported.
    abstract DeleteBy: pred:('a -> bool) -> Eventive<bool>

/// Defines a queue strategy.
[<Interface>]
type IQueueStrategy =

    /// Creates a new queue storage within the computation.
    abstract CreateStorage<'a> : unit -> Simulation<IQueueStorage<'a>>

/// Represents strategy: First Come - First Served (FCFS, or also known as FIFO).
[<Sealed>]
type FCFS =

    interface IQueueStrategy

/// Represents strategy: Last Come - First Served (LCFS, or also known as LIFO).
[<Sealed>]
type LCFS =

    interface IQueueStrategy

/// Represents strategy: Service in Random Order (SIRO).
[<Sealed>]
type SIRO =

    interface IQueueStrategy

/// Represents strategy: Static Priorities (using the priority queue).
[<Sealed>]
type StaticPriorities =

    interface IQueueStrategy

/// This module contains functions for working with the queue strategies.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module QueueStrategy =

    /// Returns strategy: First Come - First Served (FCFS, or also known as FIFO).
    [<CompiledName ("FCFS")>]
    val FCFS: FCFS

    /// Returns strategy: Last Come - First Served (LCFS, or also known as LIFO).
    [<CompiledName ("LCFS")>]
    val LCFS: LCFS

    /// Returns strategy: Service in Random Order (SIRO).
    [<CompiledName ("SIRO")>]
    val SIRO: SIRO

    /// Returns strategy: Static Priorities (using the priority queue).
    [<CompiledName ("StaticPriorities")>]
    val staticPriorities: StaticPriorities
