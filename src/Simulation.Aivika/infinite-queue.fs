
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

open Simulation.Aivika.Basic

type InfiniteQueue<'a> =
    { StoringStrategy: IQueueStrategy;
      OutputStrategy: IQueueStrategy;
      Store: IQueueStorage<InfiniteQueueItem<'a>>;
      OutputRes: Resource;
      mutable Count: int;
      mutable CountStats: TimingStats<int>;
      mutable StoreCount: int;
      mutable OutputRequestCount: int;
      mutable OutputCount: int;
      mutable WaitTime: SamplingStats<Time>;
      mutable OutputWaitTime: SamplingStats<Time>;
      EnqueueStoredSource: SignalSource<'a>;
      DequeueRequestedSource: SignalSource<unit>;
      DequeueExtractedSource: SignalSource<'a> }

and InfiniteQueueItem<'a> =
    { Value: 'a;
      StoringTime: Time }

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module InfiniteQueue =

    [<CompiledName ("StoringStrategy")>]
    let storingStrategy (q: InfiniteQueue<'a>) = q.StoringStrategy

    [<CompiledName ("OutputStrategy")>]
    let outputStrategy (q: InfiniteQueue<'a>) = q.OutputStrategy

    [<CompiledName ("Create")>]
    let create<'sm, 'so, 'a 
                    when 'sm :> IQueueStrategy and
                         'so :> IQueueStrategy> 
                            (storingStrat: 'sm) 
                            (outputStrat: 'so) : 
                                Eventive<InfiniteQueue<'a>> =
        eventive {
            let! t  = Dynamics.time |> Dynamics.lift
            let! qm = storingStrat.CreateStorage () |> Simulation.lift
            let! ro = Resource.createWithMaxCount outputStrat 0 None |> Simulation.lift
            let! s1 = SignalSource.create |> Simulation.lift
            let! s2 = SignalSource.create |> Simulation.lift
            let! s3 = SignalSource.create |> Simulation.lift
            let cs  = TimingStats.emptyInts |> TimingStats.add t 0
            return { StoringStrategy = storingStrat;
                     OutputStrategy = outputStrat;
                     Store = qm;
                     OutputRes = ro;
                     Count = 0;
                     CountStats = cs;
                     StoreCount = 0;
                     OutputRequestCount = 0;
                     OutputCount = 0;
                     WaitTime = SamplingStats.emptyFloats;
                     OutputWaitTime = SamplingStats.emptyFloats;
                     EnqueueStoredSource = s1;
                     DequeueRequestedSource = s2;
                     DequeueExtractedSource = s3 }
        }
        
    [<CompiledName ("CreateUsingFCFS")>]
    let createUsingFCFS<'a> : Eventive<InfiniteQueue<'a>> =
        create QueueStrategy.FCFS 
               QueueStrategy.FCFS 
        
    [<CompiledName ("CreateUsingLCFS")>]
    let createUsingLCFS<'a> : Eventive<InfiniteQueue<'a>> =
        create QueueStrategy.LCFS 
               QueueStrategy.FCFS 
        
    [<CompiledName ("CreateUsingSIRO")>]
    let createUsingSIRO<'a> : Eventive<InfiniteQueue<'a>> =
        create QueueStrategy.SIRO 
               QueueStrategy.FCFS 
        
    [<CompiledName ("CreateUsingPriorities")>]
    let createUsingPriorities<'a> : Eventive<InfiniteQueue<'a>> =
        create QueueStrategy.staticPriorities 
               QueueStrategy.FCFS 

    [<CompiledName ("EnqueueStored")>]
    let enqueueStored (q: InfiniteQueue<'a>) =
        SignalSource.publish q.EnqueueStoredSource

    [<CompiledName ("DequeueRequested")>]
    let dequeueRequested (q: InfiniteQueue<'a>) =
        SignalSource.publish q.DequeueRequestedSource

    [<CompiledName ("DequeueExtracted")>]
    let dequeueExtracted (q: InfiniteQueue<'a>) =
        SignalSource.publish q.DequeueExtractedSource

    /// Stores the item.
    let private enqueueStore (a: 'a) (q: InfiniteQueue<'a>) =
        Eventive (fun p ->
            let t = p.Time
            let i = { Value = a; StoringTime = t }
            q.Store.Enqueue (i)
                |> invokeEventive p
            q.Count <- q.Count + 1
            q.CountStats <- q.CountStats |> TimingStats.add t q.Count
            q.StoreCount <- q.StoreCount + 1
            Resource.releaseWithinEventive q.OutputRes
                |> invokeEventive p
            SignalSource.trigger i.Value q.EnqueueStoredSource
                |> invokeEventive p)

    /// Stores with the priority the item.
    let private enqueueStoreWithPriority pm a (q: InfiniteQueue<'a>) =
        Eventive (fun p ->
            let t = p.Time
            let i = { Value = a; StoringTime = t }
            q.Store.Enqueue (pm, i)
                |> invokeEventive p
            q.Count <- q.Count + 1
            q.CountStats <- q.CountStats |> TimingStats.add t q.Count
            q.StoreCount <- q.StoreCount + 1
            Resource.releaseWithinEventive q.OutputRes
                |> invokeEventive p
            SignalSource.trigger i.Value q.EnqueueStoredSource
                |> invokeEventive p)

    /// Accepts the dequeueing request and returns the current simulation time.
    let private dequeueRequest (q: InfiniteQueue<'a>) =
        Eventive (fun p ->
            q.OutputRequestCount <- q.OutputRequestCount + 1
            SignalSource.trigger () q.DequeueRequestedSource
                |> invokeEventive p
            p.Time)

    /// Updates the statistics for the output wait time of the dequeueing operation and the wait time of storing in the queue.
    let private dequeueStat t' (i: InfiniteQueueItem<'a>) (q: InfiniteQueue<'a>) =
        Eventive (fun p ->
            let t1 = i.StoringTime
            let t  = p.Time
            q.OutputWaitTime <-
                q.OutputWaitTime
                    |> SamplingStats.add (t - t')
            q.WaitTime <-
                q.WaitTime
                    |> SamplingStats.add (t - t1))

    /// Extracts an item for the dequeueing request.
    let private dequeueExtract t' (q: InfiniteQueue<'a>) =
        Eventive (fun p ->
            let t = p.Time
            let i = q.Store.Dequeue ()
                        |> invokeEventive p
            q.Count <- q.Count - 1
            q.CountStats <- q.CountStats |> TimingStats.add t q.Count
            q.OutputCount <- q.OutputCount + 1
            dequeueStat t' i q
                |> invokeEventive p
            SignalSource.trigger i.Value q.DequeueExtractedSource
                |> invokeEventive p
            i.Value)

    [<CompiledName ("Dequeue")>]
    let dequeue q =
        proc {
            let! t = dequeueRequest q |> Eventive.lift
            do! Resource.request q.OutputRes
            return! dequeueExtract t q |> Eventive.lift
        }

    [<CompiledName ("DequeueWithOutputPriority")>]
    let dequeueWithOutputPriority po q =
        proc {
            let! t = dequeueRequest q |> Eventive.lift
            do! Resource.requestWithPriority po  q.OutputRes
            return! dequeueExtract t q |> Eventive.lift
        }

    [<CompiledName ("TryDequeue")>]
    let tryDequeue q = 
        eventive {
            let! x = Resource.tryRequestWithinEventive q.OutputRes
            if x then
                let! t = dequeueRequest q
                return! dequeueExtract t q |> Eventive.map Some
            else
                return None
        }

    [<CompiledName ("Enqueue")>]
    let enqueue a q = 
        enqueueStore a q

    [<CompiledName ("EnqueueWithStoringPriority")>]
    let enqueueWithStoringPriority pm a q =
        enqueueStoreWithPriority pm a q

    [<CompiledName ("IsEmpty")>]
    let isEmpty q =
        Eventive (fun p -> q.Count = 0)
        
    [<CompiledName ("Count")>]
    let count q =
        Eventive (fun p -> q.Count)

    [<CompiledName ("CountStats")>]
    let countStats q =
        Eventive (fun p -> q.CountStats)

    [<CompiledName ("CountChanged_")>]
    let countChanged_ q =
        let s1 = enqueueStored q |> Signal.map (fun a -> ())
        let s2 = dequeueExtracted q |> Signal.map (fun a -> ())
        Signal.merge s1 s2

    [<CompiledName ("CountChanged")>]
    let countChanged q =
        countChanged_ q |> Signal.map (fun () -> q.Count)
                            
    [<CompiledName ("StoreCount")>]
    let storeCount q =
        Eventive (fun p -> q.StoreCount)

    [<CompiledName ("StoreCountChanged_")>]
    let storeCountChanged_ q =
        enqueueStored q |> Signal.map (fun a -> ())

    [<CompiledName ("StoreCountChanged")>]
    let storeCountChanged q =
        storeCountChanged_ q |> Signal.map (fun () -> q.StoreCount)
                            
    [<CompiledName ("OutputRequestCount")>]
    let outputRequestCount q =
        Eventive (fun p -> q.OutputRequestCount)

    [<CompiledName ("OutputRequestCountChanged_")>]
    let outputRequestCountChanged_ q =
        dequeueRequested q

    [<CompiledName ("OutputRequestCountChanged")>]
    let outputRequestCountChanged q =
        outputRequestCountChanged_ q |> Signal.map (fun () -> q.OutputRequestCount)
        
    [<CompiledName ("OutputCount")>]
    let outputCount q =
        Eventive (fun p -> q.OutputCount)

    [<CompiledName ("OutputCountChanged_")>]
    let outputCountChanged_ q =
        dequeueExtracted q |> Signal.map (fun a -> ())

    [<CompiledName ("OutputCountChanged")>]
    let outputCountChanged q =
        outputCountChanged_ q |> Signal.map (fun () -> q.OutputCount)

    [<CompiledName ("StoreRate")>]
    let storeRate q =
        Eventive (fun p ->
            float q.StoreCount / (p.Time - p.Specs.StartTime))

    [<CompiledName ("OutputRequestRate")>]
    let outputRequestRate q =
        Eventive (fun p ->
            float q.OutputRequestCount / (p.Time - p.Specs.StartTime)) 

    [<CompiledName ("OutputRate")>]
    let outputRate q =
        Eventive (fun p ->
            float q.OutputCount / (p.Time - p.Specs.StartTime))
        
    [<CompiledName ("WaitTime")>]
    let waitTime q =
        Eventive (fun p -> q.WaitTime) 

    [<CompiledName ("WaitTimeChanged_")>]
    let waitTimeChanged_ q =
        dequeueExtracted q |> Signal.map (fun a -> ())

    [<CompiledName ("WaitTimeChanged")>]
    let waitTimeChanged q =
        waitTimeChanged_ q |> Signal.map (fun () -> q.WaitTime)
        
    [<CompiledName ("OutputWaitTime")>]
    let outputWaitTime q =
        Eventive (fun p -> q.OutputWaitTime)

    [<CompiledName ("OutputWaitTimeChanged_")>]
    let outputWaitTimeChanged_ q =
        dequeueExtracted q |> Signal.map (fun a -> ())

    [<CompiledName ("OutputWaitTimeChanged")>]
    let outputWaitTimeChanged q =
        outputWaitTimeChanged_ q |> Signal.map (fun () -> q.OutputWaitTime)

    [<CompiledName ("Rate")>]
    let rate q =
        Eventive (fun p ->
            let x1 = q.CountStats
            let x2 = q.WaitTime
            x1.Mean / x2.Mean)

    [<CompiledName ("RateChanged_")>]
    let rateChanged_ q =
        let s1 = enqueueStored q |> Signal.map (fun a -> ())
        let s2 = dequeueExtracted q |> Signal.map (fun a -> ())
        Signal.merge s1 s2

    [<CompiledName ("RateChanged")>]
    let rateChanged q = 
        rateChanged_ q |> Signal.mapc (fun () -> rate q)

    [<CompiledName ("Changed_")>]
    let changed_ q =
        let s1 = enqueueStored q |> Signal.map (fun a -> ())
        let s2 = dequeueRequested q
        let s3 = dequeueExtracted q |> Signal.map (fun a -> ())
        Signal.concat [s1; s2; s3]
