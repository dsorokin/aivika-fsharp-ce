
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

namespace Simulation.Aivika.Basic

open System

open Simulation.Aivika
open Simulation.Aivika.Basic

type Queue<'a> =
    { MaxCount: int;
      InputStrategy: IQueueStrategy;
      StoringStrategy: IQueueStrategy;
      OutputStrategy: IQueueStrategy;
      InputRes: Resource;
      Store: IQueueStorage<'a>;
      OutputRes: Resource;
      mutable Count: int }

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
                                Simulation<Queue<'a>> =
        simulation {
            let! ri = Resource.createWithMaxCount inputStrat maxCount (Some maxCount) |> Simulation.lift
            let! qm = storingStrat.CreateStorage () |> Simulation.lift
            let! ro = Resource.createWithMaxCount outputStrat 0 (Some maxCount) |> Simulation.lift
            return { MaxCount = maxCount;
                     InputStrategy = inputStrat;
                     StoringStrategy = storingStrat;
                     OutputStrategy = outputStrat;
                     InputRes = ri;
                     Store = qm;
                     OutputRes = ro;
                     Count = 0 }
        }
        
    [<CompiledName ("CreateUsingFCFS")>]
    let createUsingFCFS<'a> (maxCount: int) : Simulation<Queue<'a>> =
        create QueueStrategy.FCFS 
               QueueStrategy.FCFS 
               QueueStrategy.FCFS 
               maxCount
        
    [<CompiledName ("CreateUsingLCFS")>]
    let createUsingLCFS<'a> (maxCount: int) : Simulation<Queue<'a>> =
        create QueueStrategy.FCFS 
               QueueStrategy.LCFS 
               QueueStrategy.FCFS 
               maxCount
        
    [<CompiledName ("CreateUsingSIRO")>]
    let createUsingSIRO<'a> (maxCount: int) : Simulation<Queue<'a>> =
        create QueueStrategy.FCFS 
               QueueStrategy.SIRO 
               QueueStrategy.FCFS 
               maxCount
        
    [<CompiledName ("CreateUsingPriorities")>]
    let createUsingPriorities<'a> (maxCount: int) : Simulation<Queue<'a>> =
        create QueueStrategy.FCFS 
               QueueStrategy.staticPriorities 
               QueueStrategy.FCFS 
               maxCount

    /// Stores the item.
    let private enqueueStore (item: 'a) (q: Queue<'a>) =
        Eventive (fun p ->
            q.Store.Enqueue (item)
                |> invokeEventive p
            q.Count <- q.Count + 1
            Resource.releaseWithinEventive q.OutputRes
                |> invokeEventive p)

    /// Stores with the priority the item.
    let private enqueueStoreWithPriority pm (item: 'a) (q: Queue<'a>) =
        Eventive (fun p ->
            q.Store.Enqueue (pm, item)
                |> invokeEventive p
            q.Count <- q.Count + 1
            Resource.releaseWithinEventive q.OutputRes
                |> invokeEventive p)

    /// Post extracts an item for the dequeueing request.
    let private dequeuePostExtract (q: Queue<'a>) (item: 'a) =
        Eventive (fun p ->
            let t = p.Time
            q.Count <- q.Count - 1
            Resource.releaseWithinEventive q.InputRes
                |> invokeEventive p
            item)

    /// Extracts an item for the dequeueing request.
    let private dequeueExtract (q: Queue<'a>) =
        Eventive (fun p ->
            let i = q.Store.Dequeue ()
                        |> invokeEventive p
            dequeuePostExtract q i
                |> invokeEventive p)

    [<CompiledName ("Dequeue")>]
    let dequeue q =
        proc {
            do! Resource.request q.OutputRes
            return! dequeueExtract q |> Eventive.lift
        }

    [<CompiledName ("DequeueWithOutputPriority")>]
    let dequeueWithOutputPriority po q =
        proc {
            do! Resource.requestWithPriority po  q.OutputRes
            return! dequeueExtract q |> Eventive.lift
        }

    [<CompiledName ("TryDequeue")>]
    let tryDequeue q = 
        eventive {
            let! x = Resource.tryRequestWithinEventive q.OutputRes
            if x then
                return! dequeueExtract q |> Eventive.map Some
            else
                return None
        }

    [<CompiledName ("DeleteBy")>]
    let deleteBy pred q =
        eventive {
            let! x = Resource.tryRequestWithinEventive q.OutputRes
            if x then
                let! i = q.Store.DeleteBy pred
                match i with
                | None ->
                    do! Resource.releaseWithinEventive q.OutputRes
                    return None
                | Some i ->
                    return! dequeuePostExtract q i |> Eventive.map Some
            else
                return None
        }
            
    [<CompiledName ("DeleteBy_")>]
    let deleteBy_ pred q =
        deleteBy pred q |> Eventive.map ignore

    [<CompiledName ("Delete")>]
    let delete a q =
        deleteBy (fun x -> a = x) q |> Eventive.map Option.isSome

    [<CompiledName ("Delete_")>]
    let delete_ a q =
        deleteBy (fun x -> a = x) q |> Eventive.map ignore

    [<CompiledName ("Clear")>]
    let rec clear q =
        eventive {
            let! x = tryDequeue q
            match x with
            | None   -> return ()
            | Some a -> return! clear q
        }

    [<CompiledName ("Enqueue")>]
    let enqueue a q =
        proc {
            do! Resource.request q.InputRes
            do! enqueueStore a q |> Eventive.lift 
        }

    [<CompiledName ("EnqueueWithInputPriority")>]
    let enqueueWithInputPriority pi a q =
        proc {
            do! Resource.requestWithPriority pi q.InputRes
            do! enqueueStore a q |> Eventive.lift 
        }

    [<CompiledName ("EnqueueWithStoringPriority")>]
    let enqueueWithStoringPriority pm a q =
        proc {
            do! Resource.request q.InputRes
            do! enqueueStoreWithPriority pm a q |> Eventive.lift 
        }

    [<CompiledName ("EnqueueWithInputStoringPriorities")>]
    let enqueueWithInputStoringPriorities pi pm a q =
        proc {
            do! Resource.requestWithPriority pi q.InputRes
            do! enqueueStoreWithPriority pm a q |> Eventive.lift 
        }

    [<CompiledName ("TryEnqueue")>]
    let tryEnqueue a q =
        eventive {
            let! x = Resource.tryRequestWithinEventive q.InputRes
            if x then
                do! enqueueStore a q
                return true
            else
                return false
        }

    [<CompiledName ("TryEnqueueWithStoringPriority")>]
    let tryEnqueueWithStoringPriority pm a q =
        eventive {
            let! x = Resource.tryRequestWithinEventive q.InputRes
            if x then
                do! enqueueStoreWithPriority pm a q
                return true
            else
                return false
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
