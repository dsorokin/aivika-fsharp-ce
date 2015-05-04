
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

type Queue<'a> =
    { MaxCount: int;
      InputStrategy: IQueueStrategy;
      StoringStrategy: IQueueStrategy;
      OutputStrategy: IQueueStrategy;
      InputRes: Resource;
      Store: IQueueStorage<QueueItem<'a>>;
      OutputRes: Resource;
      mutable Count: int;
      mutable CountStats: TimingStats<int>;
      mutable LostCount: int;
      mutable InputCount: int;
      mutable StoreCount: int;
      mutable OutputRequestCount: int;
      mutable OutputCount: int;
      mutable WaitTime: SamplingStats<Time>;
      mutable TotalWaitTime: SamplingStats<Time>;
      mutable InputWaitTime: SamplingStats<Time>;
      mutable OutputWaitTime: SamplingStats<Time>;
      EnqueueInitiatedSource: SignalSource<'a>;
      EnqueueLostSource: SignalSource<'a>;
      EnqueueStoredSource: SignalSource<'a>;
      DequeueRequestedSource: SignalSource<unit>;
      DequeueExtractedSource: SignalSource<'a> }

and QueueItem<'a> =
    { Value: 'a;
      InputTime: Time;
      StoringTime: Time }

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Queue =

    [<CompiledName ("MaxCount")>]
    let maxCount (q: Queue<'a>) = q.MaxCount

    [<CompiledName ("InputStrategy")>]
    let inputStrategy (q: Queue<'a>) = q.InputStrategy

    [<CompiledName ("StoringStrategy")>]
    let storingStrategy (q: Queue<'a>) = q.StoringStrategy

    [<CompiledName ("OutputStrategy")>]
    let outputStrategy (q: Queue<'a>) = q.OutputStrategy

    [<CompiledName ("Create")>]
    let create<'si, 'sm, 'so, 'a 
                    when 'si :> IQueueStrategy and
                         'sm :> IQueueStrategy and
                         'so :> IQueueStrategy> 
                            (inputStrat: 'si) 
                            (storingStrat: 'sm) 
                            (outputStrat: 'so) 
                            (maxCount: int) : 
                                Eventive<Queue<'a>> =
        eventive {
            let! t  = Dynamics.time |> Dynamics.lift
            let! ri = Resource.createWithMaxCount inputStrat maxCount (Some maxCount) |> Simulation.lift
            let! qm = storingStrat.CreateStorage () |> Simulation.lift
            let! ro = Resource.createWithMaxCount outputStrat 0 (Some maxCount) |> Simulation.lift
            let! s1 = SignalSource.create |> Simulation.lift
            let! s2 = SignalSource.create |> Simulation.lift
            let! s3 = SignalSource.create |> Simulation.lift
            let! s4 = SignalSource.create |> Simulation.lift
            let! s5 = SignalSource.create |> Simulation.lift
            let cs  = TimingStats.emptyInts |> TimingStats.add t 0
            return { MaxCount = maxCount;
                     InputStrategy = inputStrat;
                     StoringStrategy = storingStrat;
                     OutputStrategy = outputStrat;
                     InputRes = ri;
                     Store = qm;
                     OutputRes = ro;
                     Count = 0;
                     CountStats = cs;
                     LostCount = 0;
                     InputCount = 0;
                     StoreCount = 0;
                     OutputRequestCount = 0;
                     OutputCount = 0;
                     WaitTime = SamplingStats.emptyFloats;
                     TotalWaitTime = SamplingStats.emptyFloats;
                     InputWaitTime = SamplingStats.emptyFloats;
                     OutputWaitTime = SamplingStats.emptyFloats;
                     EnqueueInitiatedSource = s1;
                     EnqueueLostSource = s2;
                     EnqueueStoredSource = s3;
                     DequeueRequestedSource = s4;
                     DequeueExtractedSource = s5 }
        }
        
    [<CompiledName ("CreateUsingFCFS")>]
    let createUsingFCFS<'a> (maxCount: int) : Eventive<Queue<'a>> =
        create QueueStrategy.FCFS 
               QueueStrategy.FCFS 
               QueueStrategy.FCFS 
               maxCount
        
    [<CompiledName ("CreateUsingLCFS")>]
    let createUsingLCFS<'a> (maxCount: int) : Eventive<Queue<'a>> =
        create QueueStrategy.FCFS 
               QueueStrategy.LCFS 
               QueueStrategy.FCFS 
               maxCount
        
    [<CompiledName ("CreateUsingSIRO")>]
    let createUsingSIRO<'a> (maxCount: int) : Eventive<Queue<'a>> =
        create QueueStrategy.FCFS 
               QueueStrategy.SIRO 
               QueueStrategy.FCFS 
               maxCount
        
    [<CompiledName ("CreateUsingPriorities")>]
    let createUsingPriorities<'a> (maxCount: int) : Eventive<Queue<'a>> =
        create QueueStrategy.FCFS 
               QueueStrategy.staticPriorities 
               QueueStrategy.FCFS 
               maxCount

    [<CompiledName ("EnqueueInitiated")>]
    let enqueueInitiated (q: Queue<'a>) = 
        SignalSource.publish q.EnqueueInitiatedSource

    [<CompiledName ("EnqueueStored")>]
    let enqueueStored (q: Queue<'a>) =
        SignalSource.publish q.EnqueueStoredSource

    [<CompiledName ("EnqueueLost")>]
    let enqueueLost (q: Queue<'a>) =
        SignalSource.publish q.EnqueueLostSource

    [<CompiledName ("DequeueRequested")>]
    let dequeueRequested (q: Queue<'a>) =
        SignalSource.publish q.DequeueRequestedSource

    [<CompiledName ("DequeueExtracted")>]
    let dequeueExtracted (q: Queue<'a>) =
        SignalSource.publish q.DequeueExtractedSource

    /// Initiates the process of enqueueing the item.
    let private enqueueInitiate a (q: Queue<'a>) =
        Eventive (fun p ->
            let t = p.Time
            q.InputCount <- q.InputCount + 1
            SignalSource.trigger a q.EnqueueInitiatedSource
                |> invokeEventive p
            { Value = a;
              InputTime = t;
              StoringTime = t })  // the storing time will be updated soon

    /// Updates the statistics for the input wait time of the enqueueing operation.
    let private enqueueStat (i: QueueItem<'a>) (q: Queue<'a>) =
        Eventive (fun p ->
            let t0 = i.InputTime
            let t1 = i.StoringTime
            q.InputWaitTime <-
                q.InputWaitTime
                    |> SamplingStats.add (t1 - t0))

    /// Stores the item.
    let private enqueueStore (i: QueueItem<'a>) (q: Queue<'a>) =
        Eventive (fun p ->
            let t = p.Time
            let i' = { i with StoringTime = t }  // now we have the actual time of storing
            q.Store.Enqueue (i')
                |> invokeEventive p
            q.Count <- q.Count + 1
            q.CountStats <- q.CountStats |> TimingStats.add t q.Count
            q.StoreCount <- q.StoreCount + 1
            enqueueStat i' q
                |> invokeEventive p
            Resource.releaseWithinEventive q.OutputRes
                |> invokeEventive p
            SignalSource.trigger i'.Value q.EnqueueStoredSource
                |> invokeEventive p)

    /// Stores with the priority the item.
    let private enqueueStoreWithPriority pm i (q: Queue<'a>) =
        Eventive (fun p ->
            let t = p.Time
            let i' = { i with StoringTime = t }  // now we have the actual time of storing
            q.Store.Enqueue (pm, i')
                |> invokeEventive p
            q.Count <- q.Count + 1
            q.CountStats <- q.CountStats |> TimingStats.add t q.Count
            q.StoreCount <- q.StoreCount + 1
            enqueueStat i' q
                |> invokeEventive p
            Resource.releaseWithinEventive q.OutputRes
                |> invokeEventive p
            SignalSource.trigger i'.Value q.EnqueueStoredSource
                |> invokeEventive p)

    /// Denies the enqueueing.
    let private enqueueDeny a (q: Queue<'a>) =
        Eventive (fun p ->
            q.LostCount <- q.LostCount + 1
            SignalSource.trigger a q.EnqueueLostSource
                |> invokeEventive p)

    /// Accepts the dequeueing request and returns the current simulation time.
    let private dequeueRequest (q: Queue<'a>) =
        Eventive (fun p ->
            q.OutputRequestCount <- q.OutputRequestCount + 1
            SignalSource.trigger () q.DequeueRequestedSource
                |> invokeEventive p
            p.Time)

    /// Updates the statistics for the output wait time of the dequeueing operation and the wait time of storing in the queue.
    let private dequeueStat t' (i: QueueItem<'a>) (q: Queue<'a>) =
        Eventive (fun p ->
            let t0 = i.InputTime
            let t1 = i.StoringTime
            let t  = p.Time
            q.OutputWaitTime <-
                q.OutputWaitTime
                    |> SamplingStats.add (t - t')
            q.TotalWaitTime <-
                q.TotalWaitTime
                    |> SamplingStats.add (t - t0)
            q.WaitTime <-
                q.WaitTime
                    |> SamplingStats.add (t - t1))

    /// Extracts an item for the dequeueing request.
    let private dequeueExtract t' (q: Queue<'a>) =
        Eventive (fun p ->
            let t = p.Time
            let i = q.Store.Dequeue ()
                        |> invokeEventive p
            q.Count <- q.Count - 1
            q.CountStats <- q.CountStats |> TimingStats.add t q.Count
            q.OutputCount <- q.OutputCount + 1
            dequeueStat t' i q
                |> invokeEventive p
            Resource.releaseWithinEventive q.InputRes
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
        proc {
            let! i = enqueueInitiate a q |> Eventive.lift
            do! Resource.request q.InputRes
            do! enqueueStore i q |> Eventive.lift 
        }

    [<CompiledName ("EnqueueWithInputPriority")>]
    let enqueueWithInputPriority pi a q =
        proc {
            let! i = enqueueInitiate a q |> Eventive.lift
            do! Resource.requestWithPriority pi q.InputRes
            do! enqueueStore i q |> Eventive.lift 
        }

    [<CompiledName ("EnqueueWithStoringPriority")>]
    let enqueueWithStoringPriority pm a q =
        proc {
            let! i = enqueueInitiate a q |> Eventive.lift
            do! Resource.request q.InputRes
            do! enqueueStoreWithPriority pm i q |> Eventive.lift 
        }

    [<CompiledName ("EnqueueWithInputStoringPriorities")>]
    let enqueueWithInputStoringPriorities pi pm a q =
        proc {
            let! i = enqueueInitiate a q |> Eventive.lift
            do! Resource.requestWithPriority pi q.InputRes
            do! enqueueStoreWithPriority pm i q |> Eventive.lift 
        }

    [<CompiledName ("TryEnqueue")>]
    let tryEnqueue a q =
        eventive {
            let! x = Resource.tryRequestWithinEventive q.InputRes
            if x then
                let! i = enqueueInitiate a q
                do! enqueueStore i q
                return true
            else
                return false
        }

    [<CompiledName ("TryEnqueueWithStoringPriority")>]
    let tryEnqueueWithStoringPriority pm a q =
        eventive {
            let! x = Resource.tryRequestWithinEventive q.InputRes
            if x then
                let! i = enqueueInitiate a q
                do! enqueueStoreWithPriority pm i q
                return true
            else
                return false
        }

    [<CompiledName ("EnqueueOrLost")>]
    let enqueueOrLost a q =
        eventive {
            let! x = Resource.tryRequestWithinEventive q.InputRes
            if x then
                let! i = enqueueInitiate a q
                do! enqueueStore i q
                return true
            else
                do! enqueueDeny a q
                return false
        }

    [<CompiledName ("EnqueueWithStoringPriorityOrLost")>]
    let enqueueWithStoringPriorityOrLost pm a q =
        eventive {
            let! x = Resource.tryRequestWithinEventive q.InputRes
            if x then
                let! i = enqueueInitiate a q
                do! enqueueStoreWithPriority pm i q
                return true
            else
                do! enqueueDeny a q
                return false
        }

    [<CompiledName ("EnqueueOrLost_")>]
    let enqueueOrLost_ a q =
        eventive {
            let! x = enqueueOrLost a q
            return ()
        }

    [<CompiledName ("EnqueueWithStoringPriorityOrLost_")>]
    let enqueueWithStoringPriorityOrLost_ pm a q =
        eventive {
            let! x = enqueueWithStoringPriorityOrLost pm a q
            return ()
        }

    [<CompiledName ("IsEmpty")>]
    let isEmpty q =
        Eventive (fun p -> q.Count = 0)

    [<CompiledName ("IsFull")>]
    let isFull q =
        Eventive (fun p -> q.Count = q.MaxCount)
        
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

    [<CompiledName ("LostCount")>]
    let lostCount q =
        Eventive (fun p -> q.LostCount)

    [<CompiledName ("LostCountChanged_")>]
    let lostCountChanged_ q =
        enqueueLost q |> Signal.map (fun a -> ())

    [<CompiledName ("LostCountChanged")>]
    let lostCountChanged q =
        lostCountChanged_ q |> Signal.map (fun () -> q.LostCount)

    [<CompiledName ("InputCount")>]
    let inputCount q =
        Eventive (fun p -> q.InputCount)

    [<CompiledName ("InputCountChanged_")>]
    let inputCountChanged_ q =
        enqueueInitiated q |> Signal.map (fun a -> ())

    [<CompiledName ("InputCountChanged")>]
    let inputCountChanged q =
        inputCountChanged_ q |> Signal.map (fun () -> q.InputCount)
                            
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

    [<CompiledName ("LoadFactor")>]
    let loadFactor q =
        Eventive (fun p -> float q.Count / float q.MaxCount)
    
    [<CompiledName ("LoadFactorChanged_")>]
    let loadFactorChanged_ q =
        let s1 = enqueueStored q |> Signal.map (fun a -> ())
        let s2 = dequeueExtracted q |> Signal.map (fun a -> ())
        Signal.merge s1 s2

    [<CompiledName ("LoadFactorChanged")>]
    let loadFactorChanged q =
        loadFactorChanged_ q |> Signal.map (fun () -> float q.Count / float q.MaxCount)

    [<CompiledName ("InputRate")>]
    let inputRate q =
        Eventive (fun p ->
            float q.InputCount / (p.Time - p.Specs.StartTime)) 

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
                                
    [<CompiledName ("TotalWaitTime")>]
    let totalWaitTime q =
        Eventive (fun p -> q.TotalWaitTime) 

    [<CompiledName ("TotalWaitTimeChanged_")>]
    let totalWaitTimeChanged_ q =
        dequeueExtracted q |> Signal.map (fun a -> ())

    [<CompiledName ("TotalWaitTimeChanged")>]
    let totalWaitTimeChanged q =
        totalWaitTimeChanged_ q |> Signal.map (fun () -> q.TotalWaitTime)
        
    [<CompiledName ("InputWaitTime")>]
    let inputWaitTime q =
        Eventive (fun p -> q.InputWaitTime) 

    [<CompiledName ("InputWaitTimeChanged_")>]
    let inputWaitTimeChanged_ q =
        enqueueStored q |> Signal.map (fun a -> ())

    [<CompiledName ("InputWaitTimeChanged")>]
    let inputWaitTimeChanged q =
        inputWaitTimeChanged_ q |> Signal.map (fun () -> q.InputWaitTime)
        
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
        let s1 = enqueueInitiated q |> Signal.map (fun a -> ())
        let s2 = enqueueStored q |> Signal.map (fun a -> ())
        let s3 = enqueueLost q |> Signal.map (fun a -> ())
        let s4 = dequeueRequested q
        let s5 = dequeueExtracted q |> Signal.map (fun a -> ())
        Signal.concat [s1; s2; s3; s4; s5]

    [<CompiledName ("WaitWhileFull")>]
    let rec waitWhileFull q =
        proc {
            let! x = isFull q |> Eventive.lift
            if x then
                do! dequeueExtracted q
                        |> Signal.map (fun a -> ()) 
                        |> Proc.await
                do! waitWhileFull q 
        }
