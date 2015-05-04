
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

open Simulation.Aivika.Collections

type PreemptibleResourceActingItem =
    { Priority: Priority;
      Id: ProcId }

type PreemptibleResourceRequestingItem =
    { Priority: Priority;
      Id: ProcId;
      Cont: FrozenCont<unit> }

type PreemptibleResourcePreemptedItem =
    { Priority: Priority;
      Id: ProcId }

type PreemptibleResourceAwaitingItem = 
    Choice<PreemptibleResourceRequestingItem, PreemptibleResourcePreemptedItem>

type PreemptibleResource =
    { MaxCount: int option;
      mutable Count: int;
      ActingQueue: PriorityQueue<PreemptibleResourceActingItem>;
      WaitQueue: PriorityQueue<PreemptibleResourceAwaitingItem> }

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module PreemptibleResource =

    [<CompiledName ("MaxCount")>]
    let maxCount r = r.MaxCount

    [<CompiledName ("Count")>]
    let count r = Eventive (fun p -> r.Count)

    [<CompiledName ("Create")>]
    let create count =
        Simulation (fun r ->
            if count < 0 then
                failwithf "The resource count cannot be negative."
            let actingQueue = PriorityQueue<_> ()
            let waitQueue = PriorityQueue<_> ()
            { MaxCount = Some count;
              Count = count;
              ActingQueue = actingQueue;
              WaitQueue = waitQueue })

    [<CompiledName ("CreateWithMaxCount")>]
    let createWithMaxCount count maxCount =
        Simulation (fun r ->
            if count < 0 then
                failwithf "The resource count cannot be negative."
            match maxCount with
            | Some maxCount when count > maxCount ->
                failwithf "The resource count cannot be greater than its maximum value."
            | _ -> ()
            let actingQueue = PriorityQueue<_> ()
            let waitQueue = PriorityQueue<_> ()
            { MaxCount = maxCount;
              Count = count;
              ActingQueue = actingQueue;
              WaitQueue = waitQueue })

    [<CompiledName ("RequestWithPriority")>]
    let rec requestWithPriority priority (r: PreemptibleResource) =
        Proc (fun pid ->
            Cont (fun c ->
                Eventive (fun p ->
                    if r.Count = 0 then
                        if r.ActingQueue.IsEmpty then
                            let c = requestWithPriority priority r
                                        |> invokeProc pid
                                        |> invokeCont c
                                        |> freezeContReentering c ()
                                        |> invokeEventive p
                            r.WaitQueue.Enqueue (priority, Choice1Of2 { Priority = priority; Id = pid; Cont = c })
                        else
                            let item0 = r.ActingQueue.FrontValue
                            let p0    = item0.Priority
                            let pid0  = item0.Id 
                            if priority < p0 then
                                r.ActingQueue.Dequeue ()
                                r.ActingQueue.Enqueue (- priority, { Priority = priority; Id = pid })
                                r.WaitQueue.Enqueue (p0, Choice2Of2 { Priority = p0; Id = pid0 })
                                Proc.beginPreemption pid0 |> invokeEventive p
                                resumeCont c () |> invokeEventive p
                            else 
                                let c = requestWithPriority priority r
                                            |> invokeProc pid
                                            |> invokeCont c
                                            |> freezeContReentering c ()
                                            |> invokeEventive p
                                r.WaitQueue.Enqueue (priority, Choice1Of2 { Priority = priority; Id = pid; Cont = c })
                    else
                        r.Count <- r.Count - 1
                        r.ActingQueue.Enqueue (- priority, { Priority = priority; Id = pid })
                        resumeCont c () |> invokeEventive p)))

    let rec private release' (r: PreemptibleResource) =
        Eventive (fun p ->
            let a  = r.Count
            let a' = 1 + a
            match r.MaxCount with
            | Some n when a' > n ->
                failwithf "The resource count cannot be greater than its maximum value."
            | _ -> ()
            let f = r.WaitQueue.IsEmpty
            if f then
                r.Count <- a'
            else
                let item = r.WaitQueue.FrontValue
                r.WaitQueue.Dequeue ()
                match item with
                | Choice1Of2 { Priority = priority; Id = pid; Cont = c } ->
                    let c = unfreezeCont c |> invokeEventive p
                    match c with
                    | None ->
                        release' r 
                            |> invokeEventive p
                    | Some c ->
                        r.ActingQueue.Enqueue (- priority, { Priority = priority; Id = pid })
                        resumeCont c ()
                            |> Eventive.enqueue p.Time
                            |> invokeEventive p
                | Choice2Of2 { Priority = priority; Id = pid } ->
                    let f = Proc.isCancelled pid |> invokeEventive p
                    match f with
                    | true ->
                        release' r 
                            |> invokeEventive p
                    | false ->
                        r.ActingQueue.Enqueue (- priority, { Priority = priority; Id = pid })
                        Proc.endPreemption pid 
                            |> invokeEventive p)

    [<CompiledName ("Release")>]
    let release (r: PreemptibleResource) =
        Proc (fun pid ->
            Cont (fun c ->
                Eventive (fun p ->
                    let f = r.ActingQueue.RemoveBy (fun item -> pid = item.Id)
                    if f then
                        release' r
                            |> invokeEventive p
                        resumeCont c ()
                            |> invokeEventive p
                    else
                        failwithf "The resource was not acquired by this process.")))

    [<CompiledName ("TakeWithPriority")>]
    let takeWithPriority priority r =
        Proc (fun pid -> 
            cont {

                do! requestWithPriority priority r
                        |> invokeProc pid

                return!
                    eventive {
                        let f = r.ActingQueue.RemoveBy (fun item -> pid = item.Id)
                        if f then
                            do! release' r
                        else
                            failwithf "The resource was not acquired by this process."
                    } |> Eventive.toDisposable
                      |> Eventive.lift
            })

    let private decCount' (r: PreemptibleResource) =
        Eventive (fun p ->
            if r.Count = 0 then
                failwithf "The resource exceeded and its count is zero."
            if r.ActingQueue.IsEmpty then
                failwithf "The resource acting queue is null."
            let item0 = r.ActingQueue.FrontValue
            let p0    = item0.Priority
            let pid0  = item0.Id 
            r.ActingQueue.Dequeue ()
            r.WaitQueue.Enqueue (p0, Choice2Of2 { Priority = p0; Id = pid0 })
            Proc.beginPreemption pid0 
                |> invokeEventive p
            r.Count <- r.Count - 1)

    [<CompiledName ("IncCount")>]
    let rec incCount n r =
        if n < 0 then
            failwithf "The resource count increment cannot be negative."
        elif n = 0 then
            eventive.Zero ()
        else
            eventive {
                do! release' r
                return! incCount (n - 1) r
            }

    [<CompiledName ("DecCount")>]
    let rec decCount n r =
        if n < 0 then
            failwithf "The resource count decrement cannot be negative."
        elif n = 0 then
            eventive.Zero ()
        else
            eventive {
                do! decCount' r
                return! decCount (n - 1) r
            }

    [<CompiledName ("AlterCount")>]
    let alterCount n r =
        if n < 0 then
            decCount (- n) r
        elif n > 0 then
            incCount n r
        else
            eventive.Zero ()

