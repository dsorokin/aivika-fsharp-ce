
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
      WaitList: IQueueStorage<FrozenCont<unit>> }  

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Resource =

    [<CompiledName ("Strategy")>]
    let strategy r = r.Strategy

    [<CompiledName ("MaxCount")>]
    let maxCount r = r.MaxCount

    [<CompiledName ("Count")>]
    let count r = Eventive (fun p -> r.Count)

    [<CompiledName ("Create")>]
    let create (strat: 's when 's :> IQueueStrategy) count =
        Eventive (fun p ->
            if count < 0 then
                failwithf "The resource count cannot be negative."
            let waitList = 
                strat.CreateStorage ()
                    |> invokeSimulation p.Run
            { Strategy = strat;
              MaxCount = Some count;
              Count = count;
              WaitList = waitList })

    [<CompiledName ("CreateWithMaxCount")>]
    let createWithMaxCount (strat: 's when 's :> IQueueStrategy) count maxCount =
        Eventive (fun p ->
            if count < 0 then
                failwithf "The resource count cannot be negative."
            match maxCount with
            | Some maxCount when count > maxCount ->
                failwithf "The resource count cannot be greater than its maximum value."
            | _ -> ()
            let waitList = 
                strat.CreateStorage ()
                    |> invokeSimulation p.Run
            { Strategy = strat;
              MaxCount = maxCount;
              Count = count;
              WaitList = waitList })

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
                        r.WaitList.Enqueue (c) |> invokeEventive p
                    else
                        r.Count <- r.Count - 1
                        resumeCont c () |> invokeEventive p)))

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
                        r.WaitList.Enqueue (priority, c) |> invokeEventive p
                    else
                        r.Count <- r.Count - 1
                        resumeCont c () |> invokeEventive p)))

    [<CompiledName ("ReleaseWithinEventive")>]
    let rec releaseWithinEventive (r: Resource) =
        Eventive (fun p ->
            let a  = r.Count
            let a' = 1 + a
            match r.MaxCount with
            | Some n when a' > n ->
                failwithf "The resource count cannot be greater than its maximum value."
            | _ -> ()
            let f = r.WaitList.IsEmpty () |> invokeEventive p
            if f then
                r.Count <- a'
            else
                let c = r.WaitList.Dequeue () |> invokeEventive p
                let c = unfreezeCont c |> invokeEventive p
                match c with
                | None ->
                    releaseWithinEventive r 
                        |> invokeEventive p
                | Some c ->
                    resumeCont c ()
                        |> Eventive.enqueue p.Time
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
                r.Count <- r.Count - 1
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

    [<CompiledName ("IncCount")>]
    let rec incCount n r =
        if n < 0 then
            failwithf "The resource count increment cannot be negative."
        elif n = 0 then
            eventive.Zero ()
        else
            eventive {
                do! releaseWithinEventive r
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
                do! request r
                return! decCount (n - 1) r
            }