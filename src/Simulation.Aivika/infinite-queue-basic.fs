
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

type InfiniteQueue<'a> =
    { StoringStrategy: IQueueStrategy;
      OutputStrategy: IQueueStrategy;
      Store: IQueueStorage<'a>;
      OutputRes: Resource;
      mutable Count: int }

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
                                Simulation<InfiniteQueue<'a>> =
        simulation {
            let! qm = storingStrat.CreateStorage () |> Simulation.lift
            let! ro = Resource.createWithMaxCount outputStrat 0 None |> Simulation.lift
            return { StoringStrategy = storingStrat;
                     OutputStrategy = outputStrat;
                     Store = qm;
                     OutputRes = ro;
                     Count = 0 }
        }
        
    [<CompiledName ("CreateUsingFCFS")>]
    let createUsingFCFS<'a> : Simulation<InfiniteQueue<'a>> =
        create QueueStrategy.FCFS 
               QueueStrategy.FCFS 
        
    [<CompiledName ("CreateUsingLCFS")>]
    let createUsingLCFS<'a> : Simulation<InfiniteQueue<'a>> =
        create QueueStrategy.LCFS 
               QueueStrategy.FCFS 
        
    [<CompiledName ("CreateUsingSIRO")>]
    let createUsingSIRO<'a> : Simulation<InfiniteQueue<'a>> =
        create QueueStrategy.SIRO 
               QueueStrategy.FCFS 
        
    [<CompiledName ("CreateUsingPriorities")>]
    let createUsingPriorities<'a> : Simulation<InfiniteQueue<'a>> =
        create QueueStrategy.staticPriorities 
               QueueStrategy.FCFS 

    /// Stores the item.
    let private enqueueStore (a: 'a) (q: InfiniteQueue<'a>) =
        Eventive (fun p ->
            q.Store.Enqueue (a)
                |> invokeEventive p
            q.Count <- q.Count + 1
            Resource.releaseWithinEventive q.OutputRes
                |> invokeEventive p)

    /// Stores with the priority the item.
    let private enqueueStoreWithPriority pm a (q: InfiniteQueue<'a>) =
        Eventive (fun p ->
            q.Store.Enqueue (pm, a)
                |> invokeEventive p
            q.Count <- q.Count + 1
            Resource.releaseWithinEventive q.OutputRes
                |> invokeEventive p)

    /// Post extracts an item for the dequeueing request.
    let private dequeuePostExtract (q: InfiniteQueue<'a>) (item: 'a) =
        Eventive (fun p ->
            q.Count <- q.Count - 1
            item)

    /// Extracts an item for the dequeueing request.
    let private dequeueExtract (q: InfiniteQueue<'a>) =
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
