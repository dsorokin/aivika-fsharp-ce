
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
        
/// Represents an agent.     
type [<Sealed>] Agent =

    /// Creates an agent within the computation.
    static member Create: unit -> Simulation<Agent>

    /// Returns the selected active state.
    member SelectedState: Eventive<AgentState option>
    
    /// Returns a signal that notifies about every change of the selected state.
    member SelectedStateChanged: Signal<AgentState option>
    
    /// Returns a signal that notifies about every change of the selected state.
    member SelectedStateChanged_: Signal<unit>
        
/// Represents the agent state.
and [<AbstractClass>] AgentState =

    /// Initializes a new instance.
    internal new: agent:Agent * parent:AgentState option -> AgentState

    /// Creates a new top-level state.    
    new: agent:Agent -> AgentState
    
    /// Creates a child sub-state.
    new: parent:AgentState -> AgentState
    
    /// Returns an agent that the current state belongs to.
    member Agent: Agent
    
    /// Returns the parent state if it was defined.
    member Parent: AgentState option
    
    /// Selects the state, activating and deactivating other states of the agent as required.
    member Select: unit -> Eventive<unit>
    
    /// Returns the activation computation defined for the state.
    abstract Activate: unit -> Eventive<unit>
    
    /// Returns the deactivation computation defined for the state.
    abstract Deactivate: unit -> Eventive<unit>
    
    /// Returns the next state to be selected implicitly if the current state is directly selected.
    abstract Transit: unit -> Eventive<AgentState option>
    
    default Activate: unit -> Eventive<unit>
    
    default Deactivate: unit -> Eventive<unit>
    
    default Transit: unit -> Eventive<AgentState option>
    
    /// Adds to the state a timeout handler that will be actuated in the specified time period, if the state will remain active till that time.
    member AddTimeout: dt:Time * handler:Eventive<unit> -> Eventive<unit>
    
    /// Adds to the state a timer handler that will be repeatedly actuated in the specified time period, while the state will remain active.
    member AddTimer: dt:Eventive<Time> * handler:Eventive<unit> -> Eventive<unit>
    
/// The module contains useful functions for working with the agents.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Agent =

    /// Creates an agent within the computation.
    [<CompiledName ("Create")>]
    val create: Simulation<Agent>
    
    /// Returns the selected active state.
    [<CompiledName ("SelectedState")>]
    val selectedState: agent:Agent -> Eventive<AgentState option>
    
    /// Returns a signal that notifies about every change of the selected state.
    [<CompiledName ("SelectedStateChanged")>]
    val selectedStateChanged: agent:Agent -> Signal<AgentState option>
    
    /// Returns a signal that notifies about every change of the selected state.
    [<CompiledName ("SelectedStateChanged_")>]
    val selectedStateChanged_: agent:Agent -> Signal<unit>

/// The module contains useful functions for working with the agent states.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module AgentState =

    /// Returns an agent that the specified state belongs to.
    [<CompiledName ("Agent")>]
    val agent: state:AgentState -> Agent
    
    /// Returns the parent state if it was defined.
    [<CompiledName ("AgentState")>]
    val parent: state:AgentState -> AgentState option
     
    /// Selects the specified state, activating and deactivating other states of the agent as required.
    [<CompiledName ("Select")>]
    val select: state:AgentState -> Eventive<unit>
   
    /// Adds to the state a timeout handler that will be actuated in the specified time period, if the state will remain active till that time.
    [<CompiledName ("AddTimeout")>]
    val addTimeout: dt:Time -> handler:Eventive<unit> -> state:AgentState -> Eventive<unit>
     
    /// Adds to the state a timer handler that will be repeatedly actuated in the specified time period, while the state will remain active.
    [<CompiledName ("AddTimer")>]
    val addTimer: dt:Eventive<Time> -> handler:Eventive<unit> -> state:AgentState -> Eventive<unit>
