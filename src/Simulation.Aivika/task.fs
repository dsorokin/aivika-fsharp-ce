
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

type Task<'a> =
    { Id: ProcId;
      Result: ref<TaskResult<'a> option>;
      ResultReceived: Signal<TaskResult<'a>> }

and TaskResult<'a> = 
    | TaskCompleted of 'a
    | TaskError of exn
    | TaskCancelled

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Task =

    [<CompiledName ("TaskId")>]
    let taskId t = t.Id

    [<CompiledName ("TryGetResult")>]
    let tryGetResult t =
        Eventive (fun p -> !t.Result)
    
    [<CompiledName ("Result")>]
    let result t = proc {
        match !t.Result with
        | Some x -> return x
        | None ->
            let! x = Proc.await t.ResultReceived
            return x
    }
    
    [<CompiledName ("ResultReceived")>]
    let resultReceived t = t.ResultReceived

    [<CompiledName ("Cancel")>]
    let cancel t = Proc.cancelUsingId t.Id
    
    [<CompiledName ("IsCancelled")>]
    let isCancelled t = Proc.isCancelled t.Id
    
    [<CompiledName ("ToProc")>]
    let toProc t = proc {
        let! x = proc {
                let! pid = Proc.id
                let h () = cancel t
                use! x =
                    Proc.cancelling pid
                        |> Signal.subscribe h
                        |> Eventive.lift
                return! result t
            }
        match x with
        | TaskCompleted a -> return a
        | TaskError e -> return! raise e
        | TaskCancelled -> return! Proc.cancel
    }

    [<CompiledName ("ParResult")>]
    let parResult t1 t2 = proc {
        match !t1.Result with
        | Some x1 -> return (x1, t2)
        | None ->
            match !t2.Result with
            | Some x2 -> return (x2, t1)
            | None ->
                let s1 = t1.ResultReceived |> Signal.map Choice1Of2
                let s2 = t2.ResultReceived |> Signal.map Choice2Of2
                let! x = Signal.merge s1 s2 |> Proc.await
                match x with
                | Choice1Of2 x1 -> return (x1, t2)
                | Choice2Of2 x2 -> return (x2, t1)
    }
    
    [<CompiledName ("ToParProc")>]
    let toParProc t1 t2 = proc {
        let! (x, t) = proc {
                let! pid = Proc.id
                let h () = eventive {
                        do! cancel t1
                        do! cancel t2
                    }
                use! x =
                    Proc.cancelling pid
                        |> Signal.subscribe h
                        |> Eventive.lift
                return! parResult t1 t2
            }
        match x with
        | TaskCompleted a -> return (a, t)
        | TaskError e ->
            do! cancel t |> Eventive.lift
            return! raise e
        | TaskCancelled ->
            do! cancel t |> Eventive.lift
            return! Proc.cancel
    }

    /// <summary>Creates a task by the specified process and its identifier.</summary>    
    let private createUsingId pid p = eventive {
        
        let r = ref None
        let! s = SignalSource.create |> Simulation.lift
        
        let t = { Id = pid;
                  Result = r;
                  ResultReceived = SignalSource.publish s }
        
        let m = proc {
        
            let v = ref TaskCancelled
            let! h = 
                eventive {
                    r := Some !v
                    do! SignalSource.trigger !v s
                } |> Eventive.toDisposable
                  |> Eventive.lift
        
            try
                try
                    let! a = p
                    v := TaskCompleted a
                with
                | e -> v := TaskError e
            finally
                h.Dispose ()
        }
        
        return (t, m)
    }
    
    [<CompiledName ("RunUsingId")>]
    let runUsingId pid p = eventive {
        let! (t, m) = createUsingId pid p
        do! Proc.runUsingId pid m
        return t
    }

    [<CompiledName ("Run")>]
    let run p = eventive {
        let! pid = Proc.createId |> Simulation.lift
        return! runUsingId pid p
    }

    [<CompiledName ("EnqueueUsingId")>]
    let enqueueUsingId time pid p = eventive {
        let! (t, m) = createUsingId pid p
        do! Proc.enqueueUsingId time pid m
        return t
    } 
    
    [<CompiledName ("Enqueue")>]
    let enqueue time p = eventive {
        let! pid = Proc.createId |> Simulation.lift
        return! enqueueUsingId time pid p
    }

    [<CompiledName ("SpawnUsingIdWith")>]
    let spawnUsingIdWith cancellation pid p = proc {
        let! (t, m) = createUsingId pid p |> Eventive.lift
        do! Proc.spawnUsingIdWith cancellation pid m
        return t
    } 
    
    [<CompiledName ("SpawnWith")>]
    let spawnWith cancellation p = proc {
        let! pid = Proc.createId |> Simulation.lift
        return! spawnUsingIdWith cancellation pid p
    }

    [<CompiledName ("SpawnUsingId")>]
    let spawnUsingId pid p = spawnUsingIdWith CancelTogether pid p

    [<CompiledName ("Spawn")>]
    let spawn p = spawnWith CancelTogether p
