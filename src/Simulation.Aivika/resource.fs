
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

type Resource =
    { Strategy: IQueueStrategy;
      MaxCount: int option;
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
      WaitList: IQueueStorage<ResourceItem> }

and ResourceItem =
    { Time: float;
      Cont: FrozenCont<unit> }

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Resource =

    [<CompiledName ("Strategy")>]
    let strategy r = r.Strategy

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
    let createWithMaxCount (strat: 's when 's :> IQueueStrategy) count maxCount =
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
            let utilisationCountSource =
                SignalSource.create
                    |> invokeSimulation p.Run
            let queueCountSource =
                SignalSource.create
                    |> invokeSimulation p.Run
            let waitTimeSource =
                SignalSource.create
                    |> invokeSimulation p.Run
            let waitList = 
                strat.CreateStorage ()
                    |> invokeSimulation p.Run
            { Strategy = strat;
              MaxCount = maxCount;
              Count = count;
              CountStats = countStats;
              CountSource = countSource;
              UtilisationCount = 0;
              UtilisationCountStats = TimingStats.emptyInts;
              UtilisationCountSource = utilisationCountSource;
              QueueCount = 0;
              QueueCountStats = TimingStats.emptyInts;
              QueueCountSource = queueCountSource;
              TotalWaitTime = 0.0;
              WaitTime = SamplingStats.emptyFloats;
              WaitTimeSource = waitTimeSource;
              WaitList = waitList })

    [<CompiledName ("Create")>]
    let create (strat: 's when 's :> IQueueStrategy) count =
        createWithMaxCount strat count (Some count)

    [<CompiledName ("CreateUsingFCFS")>]
    let createUsingFCFS count = 
        create QueueStrategy.FCFS count

    [<CompiledName ("CreateWithMaxCountUsingFCFS")>]
    let createWithMaxCountUsingFCFS count maxCount = 
        createWithMaxCount QueueStrategy.FCFS count maxCount

    [<CompiledName ("CreateUsingLCFS")>]
    let createUsingLCFS count = 
        create QueueStrategy.LCFS count

    [<CompiledName ("CreateWithMaxCountUsingLCFS")>]
    let createWithMaxCountUsingLCFS count maxCount = 
        createWithMaxCount QueueStrategy.LCFS count maxCount

    [<CompiledName ("CreateUsingSIRO")>]
    let createUsingSIRO count = 
        create QueueStrategy.SIRO count

    [<CompiledName ("CreateWithMaxCountUsingSIRO")>]
    let createWithMaxCountUsingSIRO count maxCount = 
        createWithMaxCount QueueStrategy.SIRO count maxCount

    [<CompiledName ("CreateUsingPriorities")>]
    let createUsingPriorities count = 
        create QueueStrategy.staticPriorities count

    [<CompiledName ("CreateWithMaxCountUsingPriorities")>]
    let createWithMaxCountUsingPriorities count maxCount = 
        createWithMaxCount QueueStrategy.staticPriorities count maxCount

    let private updateCount (r: Resource) delta =
        Eventive (fun p ->
            let a  = r.Count
            let a' = a + delta
            r.Count <- a'
            r.CountStats <- r.CountStats |> TimingStats.add p.Time a'
            r.CountSource 
                |> SignalSource.trigger a'
                |> invokeEventive p)

    let private updateUtilisationCount (r: Resource) delta =
        Eventive (fun p ->
            let a  = r.UtilisationCount
            let a' = a + delta
            r.UtilisationCount <- a'
            r.UtilisationCountStats <- r.UtilisationCountStats |> TimingStats.add p.Time a'
            r.UtilisationCountSource 
                |> SignalSource.trigger a'
                |> invokeEventive p)

    let private updateQueueCount (r: Resource) delta =
        Eventive (fun p ->
            let a  = r.QueueCount
            let a' = a + delta
            r.QueueCount <- a'
            r.QueueCountStats <- r.QueueCountStats |> TimingStats.add p.Time a'
            r.QueueCountSource 
                |> SignalSource.trigger a'
                |> invokeEventive p)

    let private updateWaitTime (r: Resource) delta =
        Eventive (fun p ->
            let a  = r.TotalWaitTime
            let a' = a + delta
            r.TotalWaitTime <- a'
            r.WaitTime <- r.WaitTime |> SamplingStats.add delta
            r.WaitTimeSource 
                |> SignalSource.trigger ()
                |> invokeEventive p)

    [<CompiledName ("Request")>]
    let rec request (r: Resource) =
        Proc (fun pid ->
            Cont (fun c ->
                Eventive (fun p ->
                    if r.Count = 0 then
                        let c = request r
                                    |> invokeProc pid
                                    |> invokeCont c
                                    |> freezeContReentering c ()
                                    |> invokeEventive p
                        r.WaitList.Enqueue ({ Time = p.Time; Cont = c })
                            |> invokeEventive p
                        updateQueueCount r 1
                            |> invokeEventive p
                    else
                        updateWaitTime r 0.0 
                            |> invokeEventive p
                        updateCount r (-1) 
                            |> invokeEventive p
                        updateUtilisationCount r 1
                            |> invokeEventive p
                        resumeCont c () 
                            |> invokeEventive p)))

    [<CompiledName ("RequestWithPriority")>]
    let rec requestWithPriority priority (r: Resource) =
        Proc (fun pid ->
            Cont (fun c ->
                Eventive (fun p ->
                    if r.Count = 0 then
                        let c = requestWithPriority priority r
                                    |> invokeProc pid
                                    |> invokeCont c
                                    |> freezeContReentering c ()
                                    |> invokeEventive p
                        r.WaitList.Enqueue (priority, { Time = p.Time; Cont = c }) 
                            |> invokeEventive p
                        updateQueueCount r 1 
                            |> invokeEventive p
                    else
                        updateWaitTime r 0.0 
                            |> invokeEventive p
                        updateCount r (-1) 
                            |> invokeEventive p
                        updateUtilisationCount r 1 
                            |> invokeEventive p
                        resumeCont c () 
                            |> invokeEventive p)))

    let rec private release' (r: Resource) =
        Eventive (fun p ->
            let a  = r.Count
            let a' = 1 + a
            match r.MaxCount with
            | Some n when a' > n ->
                failwithf "The resource count cannot be greater than its maximum value."
            | _ -> ()
            let f = r.WaitList.IsEmpty () |> invokeEventive p
            if f then
                updateCount r 1 
                    |> invokeEventive p
            else
                let x = r.WaitList.Dequeue () |> invokeEventive p
                updateQueueCount r (-1) 
                    |> invokeEventive p
                let c = unfreezeCont x.Cont |> invokeEventive p
                match c with
                | None ->
                    release' r 
                        |> invokeEventive p
                | Some c ->
                    updateWaitTime r (p.Time - x.Time)
                        |> invokeEventive p
                    updateUtilisationCount r 1
                        |> invokeEventive p
                    resumeCont c ()
                        |> Eventive.enqueue p.Time
                        |> invokeEventive p)    

    [<CompiledName ("ReleaseWithinEventive")>]
    let releaseWithinEventive (r: Resource) =
        Eventive (fun p ->
            updateUtilisationCount r (-1)
                |> invokeEventive p
            release' r
                |> invokeEventive p)

    [<CompiledName ("Release")>]
    let release (r: Resource) =
        Proc (fun pid ->
            Cont (fun c ->
                Eventive (fun p ->
                    releaseWithinEventive r
                        |> invokeEventive p
                    resumeCont c ()
                        |> invokeEventive p)))

    [<CompiledName ("TryRequestWithinEventive")>]
    let tryRequestWithinEventive (r: Resource) =
        Eventive (fun p ->
            if r.Count = 0 then
                false
            else
                updateWaitTime r 0.0
                    |> invokeEventive p
                updateCount r (-1)
                    |> invokeEventive p
                updateUtilisationCount r 1
                    |> invokeEventive p
                true)

    [<CompiledName ("Take")>]
    let take r = proc {

        do! request r

        return! releaseWithinEventive r
                    |> Eventive.toDisposable
                    |> Eventive.lift
    }

    [<CompiledName ("TakeWithPriority")>]
    let takeWithPriority priority r = proc {

        do! requestWithPriority priority r

        return! releaseWithinEventive r
                    |> Eventive.toDisposable
                    |> Eventive.lift
    }

    let private decCount' (r: Resource) =
        proc {
            do! updateUtilisationCount r (-1)
                    |> Eventive.lift
            do! request r
        }

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
            proc.Zero ()
        else
            proc {
                do! decCount' r
                return! decCount (n - 1) r
            }