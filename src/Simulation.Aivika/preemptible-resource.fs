
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
      Time: float;
      Id: ProcId;
      Cont: FrozenCont<unit> }

type PreemptibleResourcePreemptedItem =
    { Priority: Priority;
      Time: float;
      Id: ProcId }

type PreemptibleResourceAwaitingItem = 
    Choice<PreemptibleResourceRequestingItem, PreemptibleResourcePreemptedItem>

type PreemptibleResource =
    { MaxCount: int option;
      mutable Count: int;
      mutable CountStats: TimingStats<int>;
      CountSource: SignalSource<int>;
      mutable UtilisationCount: int;
      mutable UtilisationCountStats: TimingStats<int>;
      UtilisationCountSource: SignalSource<int>;
      mutable QueueCount: int;
      mutable QueueCountStats: TimingStats<int>;
      QueueCountSource: SignalSource<int>;
      mutable TotalWaitTime: float;
      mutable WaitTime: SamplingStats<float>;
      WaitTimeSource: SignalSource<unit>;
      ActingQueue: PriorityQueue<PreemptibleResourceActingItem>;
      WaitQueue: PriorityQueue<PreemptibleResourceAwaitingItem> }

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module PreemptibleResource =

    [<CompiledName ("MaxCount")>]
    let maxCount r = r.MaxCount

    [<CompiledName ("Count")>]
    let count r = Eventive (fun p -> r.Count)

    [<CompiledName ("CountStats")>]
    let countStats r = Eventive (fun p -> r.CountStats)

    [<CompiledName ("CountChanged")>]
    let countChanged r = r.CountSource |> SignalSource.publish

    [<CompiledName ("CountChanged_")>]
    let countChanged_ r = countChanged r |> Signal.map (fun x -> ())

    [<CompiledName ("UtilisationCount")>]
    let utilisationCount r = Eventive (fun p -> r.UtilisationCount)

    [<CompiledName ("UtilisationCountStats")>]
    let utilisationCountStats r = Eventive (fun p -> r.UtilisationCountStats)

    [<CompiledName ("UtilisationCountChanged")>]
    let utilisationCountChanged r = r.UtilisationCountSource |> SignalSource.publish

    [<CompiledName ("UtilisationCountChanged_")>]
    let utilisationCountChanged_ r = utilisationCountChanged r |> Signal.map (fun x -> ())

    [<CompiledName ("QueueCount")>]
    let queueCount r = Eventive (fun p -> r.QueueCount)

    [<CompiledName ("QueueCountStats")>]
    let queueCountStats r = Eventive (fun p -> r.QueueCountStats)

    [<CompiledName ("QueueCountChanged")>]
    let queueCountChanged r = r.QueueCountSource |> SignalSource.publish

    [<CompiledName ("QueueCountChanged_")>]
    let queueCountChanged_ r = queueCountChanged r |> Signal.map (fun x -> ())

    [<CompiledName ("TotalWaitTime")>]
    let totalWaitTime r = Eventive (fun p -> r.TotalWaitTime)

    [<CompiledName ("WaitTime")>]
    let waitTime r = Eventive (fun p -> r.WaitTime)

    [<CompiledName ("WaitTimeChanged_")>]
    let waitTimeChanged_ r = r.WaitTimeSource |> SignalSource.publish

    [<CompiledName ("WaitTimeChanged")>]
    let waitTimeChanged r = waitTimeChanged_ r |> Signal.map (fun x -> r.WaitTime)

    [<CompiledName ("Changed_")>]
    let changed_ r =
        [countChanged_ r;
         utilisationCountChanged_ r;
         queueCountChanged_ r]
            |> Signal.concat

    [<CompiledName ("CreateWithMaxCount")>]
    let createWithMaxCount count maxCount =
        Eventive (fun p ->
            if count < 0 then
                failwithf "The resource count cannot be negative."
            match maxCount with
            | Some maxCount when count > maxCount ->
                failwithf "The resource count cannot be greater than its maximum value."
            | _ -> ()
            let countStats =
                TimingStats.emptyInts 
                    |> TimingStats.add p.Time count
            let countSource = 
                SignalSource.create 
                    |> invokeSimulation p.Run
            let utilisationCountStats =
                TimingStats.emptyInts
                    |> TimingStats.add p.Time 0
            let utilisationCountSource =
                SignalSource.create
                    |> invokeSimulation p.Run
            let queueCountStats =
                TimingStats.emptyInts
                    |> TimingStats.add p.Time 0
            let queueCountSource =
                SignalSource.create
                    |> invokeSimulation p.Run
            let waitTimeSource =
                SignalSource.create
                    |> invokeSimulation p.Run
            let actingQueue = PriorityQueue<_> ()
            let waitQueue = PriorityQueue<_> ()
            { MaxCount = maxCount;
              Count = count;
              CountStats = countStats;
              CountSource = countSource;
              UtilisationCount = 0;
              UtilisationCountStats = utilisationCountStats;
              UtilisationCountSource = utilisationCountSource;
              QueueCount = 0;
              QueueCountStats = queueCountStats;
              QueueCountSource = queueCountSource;
              TotalWaitTime = 0.0;
              WaitTime = SamplingStats.emptyFloats;
              WaitTimeSource = waitTimeSource;
              ActingQueue = actingQueue;
              WaitQueue = waitQueue })

    [<CompiledName ("Create")>]
    let create count =
        createWithMaxCount count (Some count)

    let private updateCount (r: PreemptibleResource) delta =
        Eventive (fun p ->
            let a  = r.Count
            let a' = a + delta
            r.Count <- a'
            r.CountStats <- r.CountStats |> TimingStats.add p.Time a'
            r.CountSource 
                |> SignalSource.trigger a'
                |> invokeEventive p)

    let private updateUtilisationCount (r: PreemptibleResource) delta =
        Eventive (fun p ->
            let a  = r.UtilisationCount
            let a' = a + delta
            r.UtilisationCount <- a'
            r.UtilisationCountStats <- r.UtilisationCountStats |> TimingStats.add p.Time a'
            r.UtilisationCountSource 
                |> SignalSource.trigger a'
                |> invokeEventive p)

    let private updateQueueCount (r: PreemptibleResource) delta =
        Eventive (fun p ->
            let a  = r.QueueCount
            let a' = a + delta
            r.QueueCount <- a'
            r.QueueCountStats <- r.QueueCountStats |> TimingStats.add p.Time a'
            r.QueueCountSource 
                |> SignalSource.trigger a'
                |> invokeEventive p)

    let private updateWaitTime (r: PreemptibleResource) delta =
        Eventive (fun p ->
            let a  = r.TotalWaitTime
            let a' = a + delta
            r.TotalWaitTime <- a'
            r.WaitTime <- r.WaitTime |> SamplingStats.add delta
            r.WaitTimeSource 
                |> SignalSource.trigger ()
                |> invokeEventive p)

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
                            r.WaitQueue.Enqueue (priority, Choice1Of2 { Priority = priority; Time = p.Time; Id = pid; Cont = c })
                            updateQueueCount r 1 
                                |> invokeEventive p
                        else
                            let item0 = r.ActingQueue.FrontValue
                            let p0    = item0.Priority
                            let pid0  = item0.Id 
                            if priority < p0 then
                                r.ActingQueue.Dequeue ()
                                r.ActingQueue.Enqueue (- priority, { Priority = priority; Id = pid })
                                r.WaitQueue.Enqueue (p0, Choice2Of2 { Priority = p0; Time = p.Time; Id = pid0 })
                                updateQueueCount r 1 
                                    |> invokeEventive p
                                Proc.beginPreemption pid0 
                                    |> invokeEventive p
                                resumeCont c () 
                                    |> invokeEventive p
                            else 
                                let c = requestWithPriority priority r
                                            |> invokeProc pid
                                            |> invokeCont c
                                            |> freezeContReentering c ()
                                            |> invokeEventive p
                                r.WaitQueue.Enqueue (priority, Choice1Of2 { Priority = priority; Time = p.Time; Id = pid; Cont = c })
                                updateQueueCount r 1 
                                    |> invokeEventive p
                    else
                        r.ActingQueue.Enqueue (- priority, { Priority = priority; Id = pid })
                        updateWaitTime r 0.0 
                            |> invokeEventive p
                        updateCount r (-1) 
                            |> invokeEventive p
                        updateUtilisationCount r 1 
                            |> invokeEventive p
                        resumeCont c () 
                            |> invokeEventive p)))

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
                updateCount r 1 
                    |> invokeEventive p
            else
                let item = r.WaitQueue.FrontValue
                r.WaitQueue.Dequeue ()
                updateQueueCount r (-1) 
                    |> invokeEventive p
                match item with
                | Choice1Of2 { Priority = priority; Time = t; Id = pid; Cont = c } ->
                    let c = unfreezeCont c |> invokeEventive p
                    match c with
                    | None ->
                        release' r 
                            |> invokeEventive p
                    | Some c ->
                        r.ActingQueue.Enqueue (- priority, { Priority = priority; Id = pid })
                        updateWaitTime r (p.Time - t) 
                            |> invokeEventive p
                        updateUtilisationCount r 1 
                            |> invokeEventive p
                        resumeCont c ()
                            |> Eventive.enqueue p.Time
                            |> invokeEventive p
                | Choice2Of2 { Priority = priority; Time = t; Id = pid } ->
                    let f = Proc.isCancelled pid |> invokeEventive p
                    match f with
                    | true ->
                        release' r 
                            |> invokeEventive p
                    | false ->
                        r.ActingQueue.Enqueue (- priority, { Priority = priority; Id = pid })
                        updateWaitTime r (p.Time - t) 
                            |> invokeEventive p
                        updateUtilisationCount r 1 
                            |> invokeEventive p
                        Proc.endPreemption pid 
                            |> invokeEventive p)

    [<CompiledName ("Release")>]
    let release (r: PreemptibleResource) =
        Proc (fun pid ->
            Cont (fun c ->
                Eventive (fun p ->
                    let f = r.ActingQueue.RemoveBy (fun item -> pid = item.Id)
                    if f then
                        updateUtilisationCount r (-1)
                            |> invokeEventive p
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
                            do! updateUtilisationCount r (-1)
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
            if not r.ActingQueue.IsEmpty then
                let item0 = r.ActingQueue.FrontValue
                let p0    = item0.Priority
                let pid0  = item0.Id 
                r.ActingQueue.Dequeue ()
                r.WaitQueue.Enqueue (p0, Choice2Of2 { Priority = p0; Time = p.Time; Id = pid0 })
                Proc.beginPreemption pid0 
                    |> invokeEventive p
                updateUtilisationCount r (-1)
                    |> invokeEventive p
                updateQueueCount r 1
                    |> invokeEventive p
            updateCount r (-1)
                |> invokeEventive p)

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

