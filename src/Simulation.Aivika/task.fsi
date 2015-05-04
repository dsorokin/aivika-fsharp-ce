
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

/// The task represents a process that was already started in background.
[<Sealed>]
type Task<'a>

/// Represents a result of the task.
and TaskResult<'a> = 
    /// The task was successfully completed and it returned the specified result.
    | TaskCompleted of 'a
    /// The specified exception was raised while performing the task.
    | TaskError of exn
    /// The task was cancelled.
    | TaskCancelled

/// The module contains useful functions for working with the tasks.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Task =

    /// Returns an identifier of the process that was launched in background for this task.
    [<CompiledName ("TaskId")>]
    val taskId: task:Task<'a> -> ProcId

    /// Tries to get the task result immediately without suspension.
    [<CompiledName ("TryGetResult")>]
    val tryGetResult: task:Task<'a> -> Eventive<TaskResult<'a> option>
    
    /// Returns the task result suspending the outer process if required.
    [<CompiledName ("Result")>]
    val result: task:Task<'a> -> Proc<TaskResult<'a>>
    
    /// Returns a signal that notifies about receiving the result of the task.
    [<CompiledName ("ResultReceived")>]
    val resultReceived: task:Task<'a> -> Signal<TaskResult<'a>>

    /// Cancels the task.  
    [<CompiledName ("Cancel")>]
    val cancel: task:Task<'a> -> Eventive<unit>
    
    /// Tests whether the task was cancelled.
    [<CompiledName ("IsCancelled")>]
    val isCancelled: task:Task<'a> -> Eventive<bool>
    
    /// Returns an outer process that behaves like the task itself.
    [<CompiledName ("ToProc")>]
    val toProc: task:Task<'a> -> Proc<'a>

    /// Gets the result of two parallel tasks returning the first finished result and then the next task.
    [<CompiledName ("ParResult")>]
    val parResult: task1:Task<'a> -> task2:Task<'a> -> Proc<TaskResult<'a> * Task<'a>>
    
    /// Returns an outer process for two parallel tasks.
    [<CompiledName ("ToParProc")>]
    val toParProc: task1:Task<'a> -> task2:Task<'a> -> Proc<'a * Task<'a>>

    /// Runs a new process with the specified identifier in background and returns the corresponding task immediately.
    [<CompiledName ("RunUsingId")>]
    val runUsingId: pid:ProcId -> comp:Proc<'a> -> Eventive<Task<'a>>

    /// Runs a new process in background and returns the corresponding task immediately.
    [<CompiledName ("Run")>]
    val run: comp:Proc<'a> -> Eventive<Task<'a>>

    /// Enqueues a new process that will be started at the specified time with the given identifier from the event queue and returns the corresponding task immediately.
    [<CompiledName ("EnqueueUsingId")>]
    val enqueueUsingId: time:Time -> pid:ProcId -> comp:Proc<'a> -> Eventive<Task<'a>>

    /// Enqueues a new process that will be started at the specified time from the event queue and returns the corresponding task immediately.
    [<CompiledName ("Enqueue")>]
    val enqueue: time:Time -> comp:Proc<'a> -> Eventive<Task<'a>>

    /// Runs using the specified identifier a child process in background and returns immediately the corresponding task.
    [<CompiledName ("SpawnUsingIdWith")>]
    val spawnUsingIdWith: cancellation:ContCancellation -> pid:ProcId -> comp:Proc<'a> -> Proc<Task<'a>>

    /// Runs a child process in background and returns immediately the corresponding task.
    [<CompiledName ("SpawnWith")>]
    val spawnWith: cancellation:ContCancellation -> comp:Proc<'a> -> Proc<Task<'a>>

    /// Runs using the specified identifier a child process in background and returns immediately the corresponding task.
    [<CompiledName ("SpawnUsingId")>]
    val spawnUsingId: pid:ProcId -> comp:Proc<'a> -> Proc<Task<'a>>

    /// Runs a child process in background and returns immediately the corresponding task.
    [<CompiledName ("Spawn")>]
    val spawn: comp:Proc<'a> -> Proc<Task<'a>>
    
