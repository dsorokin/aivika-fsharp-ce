
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

#nowarn "40"

open System

type AgentMode =
    | CreationMode
    | TransientMode
    | ProcessingMode

type [<Sealed>] Agent (selectedStateChangedSource: SignalSource<_>) = 

    let mutable mode  = CreationMode
    let mutable selectedState = None

    let selectedStateChanged  = SignalSource.publish selectedStateChangedSource
    let selectedStateChanged_ = Signal.map (fun a -> ()) selectedStateChanged
    
    let getSelectedState =
        Eventive (fun p -> selectedState)

    member internal x.ModeInternal 
        with get () = mode 
        and set (a) = mode <- a
        
    member internal x.SelectedStateInternal
        with get () = selectedState
        and set (a) = selectedState <- a
    
    member x.SelectedState = getSelectedState
    member x.SelectedStateChanged  = selectedStateChanged
    member x.SelectedStateChanged_ = selectedStateChanged_
    
    member internal x.TriggerSelectedStateChanged () =
        SignalSource.trigger selectedState selectedStateChangedSource

    static member Create () =
        Simulation (fun r ->
            let s = SignalSource.create 
                        |> invokeSimulation r
            Agent (s))
                        
and [<AbstractClass>] AgentState internal (agent: Agent, parent: AgentState option) as x = 

    let mutable version = 0

    static let rec fullPath (st: AgentState) acc =
        match st.Parent with
        | Some st' -> fullPath st' (st :: acc)
        | None -> st :: acc

    static let rec partitionPath path1 path2 = 
        match path1, path2 with
        | (h1 :: t1), [h2] when h1 = h2 ->
            List.rev path1, path2
        | (h1 :: t1), (h2 :: t2) when h1 = h2 -> 
            partitionPath t1 t2
        | _ -> 
            List.rev path1, path2

    static let findPath (source: AgentState option) (target: AgentState) =
    
        match source with
        | None -> ([], fullPath target [])
        | Some source ->
            if source.Agent <> target.Agent then
                failwithf "Different agents."
            let path1 = fullPath source []
            let path2 = fullPath target []
            partitionPath path1 path2

    static let rec traversePath (source: AgentState option) (target: AgentState) =
        let (path1, path2) = findPath source target
        let agent: Agent = target.Agent
        in Eventive (fun p ->
            if (not <| List.isEmpty path1) || (not <| List.isEmpty path2) then
                agent.ModeInternal <- TransientMode
                for st in path1 do
                    agent.SelectedStateInternal <- Some st
                    st.Deactivate () 
                        |> invokeEventive p
                    // it makes all timeout and timer handlers outdated
                    st.VersionInternal <- 1 + st.VersionInternal
                for st in path2 do
                    agent.SelectedStateInternal <- Some st
                    st.Activate () 
                        |> invokeEventive p
                let st' = target.Transit () 
                            |> invokeEventive p           
                match st' with
                |  None ->
                    agent.ModeInternal <- ProcessingMode
                    agent.TriggerSelectedStateChanged ()
                        |> invokeEventive p
                | Some st' ->
                    traversePath (Some target) st'
                        |> invokeEventive p)
                    
    let select =
        Eventive (fun p ->
            match agent.ModeInternal with
            | CreationMode ->
                let x0 = agent.SelectedStateInternal
                traversePath x0 x
                    |> invokeEventive p
            | TransientMode ->
                failwithf "Use the Transit method to define the transition state."
            | ProcessingMode ->
                let x0 = agent.SelectedStateInternal
                assert (Option.isSome x0)
                traversePath x0 x
                    |> invokeEventive p)

    let addTimeout (dt: Time) handler =
        Eventive (fun p ->
            let v = version
            let rec m1 =
                Eventive (fun p ->
                    let v' = version
                    if v = v' then
                        invokeEventive p handler)
            and m2 = Eventive.enqueue (p.Time + dt) m1
            invokeEventive p m2)

    let addTimer (dt: Eventive<Time>) handler =
        Eventive (fun p ->
            let v = version
            let rec m1 =
                Eventive (fun p ->
                    let v' = version
                    if v = v' then
                        invokeEventive p m2
                        invokeEventive p handler)
            and m2 =
                Eventive (fun p ->
                    let dt' = invokeEventive p dt 
                    Eventive.enqueue (p.Time + dt') m1
                        |> invokeEventive p)
            invokeEventive p m2)
   
    static let activate = eventive.Zero ()
    static let deactivate = eventive.Zero ()
    static let transit = eventive.Return (None)
      
    new (agent: Agent) = AgentState (agent, None)
            
    new (parent: AgentState) = AgentState (parent.Agent, Some parent)
    
    member x.Agent = agent
    
    member x.Parent = parent
    
    member x.VersionInternal 
        with get () = version
        and set (a) = version <- a
    
    member x.Select () = select
    
    abstract Activate: unit -> Eventive<unit>
    abstract Deactivate: unit -> Eventive<unit>
    abstract Transit: unit -> Eventive<AgentState option>

    default x.Activate () = activate
    default x.Deactivate () = deactivate
    default x.Transit () = transit
       
    member x.AddTimeout (dt, handler) = addTimeout dt handler
    member x.AddTimer (dt, handler) = addTimer dt handler
    
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Agent =

    [<CompiledName ("Create")>]
    let create = Agent.Create ()
    
    [<CompiledName ("SelectedState")>]
    let selectedState (x: Agent) = x.SelectedState
    
    [<CompiledName ("SelectedStateChanged")>]
    let selectedStateChanged (x: Agent) = x.SelectedStateChanged
    
    [<CompiledName ("SelectedStateChanged_")>]
    let selectedStateChanged_ (x: Agent) = x.SelectedStateChanged_

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module AgentState =

    [<CompiledName ("Agent")>]
    let agent (x: AgentState) = x.Agent
    
    [<CompiledName ("AgentState")>]
    let parent (x: AgentState) = x.Parent
     
    [<CompiledName ("Select")>]
    let select (x: AgentState) = x.Select ()
   
    [<CompiledName ("AddTimeout")>]
    let addTimeout dt handler (state: AgentState) = state.AddTimeout (dt, handler)
     
    [<CompiledName ("AddTimer")>]
    let addTimer dt handler (state: AgentState) = state.AddTimer (dt, handler)
    
