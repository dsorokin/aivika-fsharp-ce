
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

/// It models a server that takes input a and returns output b.
[<Sealed>]
type Server<'state, 'a, 'b>

/// The module contains useful functions for working with the servers.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Server =

    /// Creates a new server accumulating its state by the specified handling function and initial state.
    [<CompiledName ("CreateAccumPreemptible")>]
    val createAccumPreemptible: preemptible:bool -> f:('state -> 'a -> Proc<'state * 'b>) -> state:'state -> Simulation<Server<'state, 'a, 'b>>

    /// Creates a new stateless server by the specified handling function.
    [<CompiledName ("CreatePreemptible")>]
    val createPreemptible: preemptible:bool -> f:('a -> Proc<'b>) -> Simulation<Server<unit, 'a, 'b>>

    /// Creates a new non-preemptible server accumulating its state by the specified handling function and initial state.
    [<CompiledName ("CreateAccum")>]
    val createAccum: f:('state -> 'a -> Proc<'state * 'b>) -> state:'state -> Simulation<Server<'state, 'a, 'b>>

    /// Creates a new non-preemptible and stateless server by the specified handling function.
    [<CompiledName ("Create")>]
    val create: f:('a -> Proc<'b>) -> Simulation<Server<unit, 'a, 'b>>

    /// Returns a processor associated with the specified server, where all processors returned by this function updates the same statistics related to the given server.
    [<CompiledName ("Processor")>]
    val processor: server:Server<'state, 'a, 'b> -> Processor<'a, 'b>

    /// A signal triggered when the server receives a new input task.
    [<CompiledName ("InputReceived")>]
    val inputReceived: server:Server<'state, 'a, 'b> -> Signal<'a>

    /// A signal triggered when the server has just processed the task and prepared an output.
    [<CompiledName ("TaskProcessed")>]
    val taskProcessed: server:Server<'state, 'a, 'b> -> Signal<'a * 'b>

    /// A signal triggered when the server processing is preempted.
    [<CompiledName ("TaskPreemptionBeginning")>]
    val taskPreemptionBeginning: server:Server<'state, 'a, 'b> -> Signal<'a>

    /// A signal triggered when the server processing is proceeded after it was preempted earlier.
    [<CompiledName ("TaskPreemptionEnding")>]
    val taskPreemptionEnding: server:Server<'state, 'a, 'b> -> Signal<'a>

    /// A signal triggered when the server has just delivered an output after processing the specified input.
    [<CompiledName ("OutputProvided")>]
    val outputProvided: server:Server<'state, 'a, 'b> -> Signal<'a * 'b>

    /// Returns the initial state of the server.
    [<CompiledName ("InitState")>]
    val initState: server:Server<'state, 'a, 'b> -> 'state

    /// Returns the current state of the server.
    [<CompiledName ("State")>]
    val state: server:Server<'state, 'a, 'b> -> Eventive<'state>

    /// A signal triggered when the state of the server changes.
    [<CompiledName ("StateChanged_")>]
    val stateChanged_: server:Server<'state, 'a, 'b> -> Signal<unit>

    /// A signal triggered when the state of the server changes.
    [<CompiledName ("StateChanged")>]
    val stateChanged: server:Server<'state, 'a, 'b> -> Signal<'state>

    /// Returns the counted total time when the server was locked while awaiting the input.
    [<CompiledName ("TotalInputWaitTime")>]
    val totalInputWaitTime: server:Server<'state, 'a, 'b> -> Eventive<float>

    /// A signal triggered when the total input wait time changes.
    [<CompiledName ("TotalInputWaitTimeChanged_")>]
    val totalInputWaitTimeChanged_: server:Server<'state, 'a, 'b> -> Signal<unit>

    /// A signal triggered when the total input wait time changes.
    [<CompiledName ("TotalInputWaitTimeChanged")>]
    val totalInputWaitTimeChanged: server:Server<'state, 'a, 'b> -> Signal<float>

    /// Returns the counted total time spent by the server while processing the tasks.
    [<CompiledName ("TotalProcessingTime")>]
    val totalProcessingTime: server:Server<'state, 'a, 'b> -> Eventive<float>

    /// A signal triggered when the total processing time changes.
    [<CompiledName ("TotalProcessingTimeChanged_")>]
    val totalProcessingTimeChanged_: server:Server<'state, 'a, 'b> -> Signal<unit>

    /// A signal triggered when the total processing time changes.
    [<CompiledName ("TotalProcessingTimeChanged")>]
    val totalProcessingTimeChanged: server:Server<'state, 'a, 'b> -> Signal<float>

    /// Returns the counted total time when the server was locked while trying to deliver the output.
    [<CompiledName ("TotalOutputWaitTime")>]
    val totalOutputWaitTime: server:Server<'state, 'a, 'b> -> Eventive<float>

    /// A signal triggered when the total output wait time changes.
    [<CompiledName ("TotalOutputWaitTimeChanged_")>]
    val totalOutputWaitTimeChanged_: server:Server<'state, 'a, 'b> -> Signal<unit>

    /// A signal triggered when the total output wait time changes.
    [<CompiledName ("TotalOutputWaitTimeChanged")>]
    val totalOutputWaitTimeChanged: server:Server<'state, 'a, 'b> -> Signal<float>

    /// Returns the counted total time spent by the server while its processing was preempted.
    [<CompiledName ("TotalPreemptionTime")>]
    val totalPreemptionTime: server:Server<'state, 'a, 'b> -> Eventive<float>

    /// A signal triggered when the total preemption time changes.
    [<CompiledName ("TotalPreemptionTimeChanged_")>]
    val totalPreemptionTimeChanged_: server:Server<'state, 'a, 'b> -> Signal<unit>

    /// A signal triggered when the total preemption time changes.
    [<CompiledName ("TotalPreemptionTimeChanged")>]
    val totalPreemptionTimeChanged: server:Server<'state, 'a, 'b> -> Signal<float>

    /// Returns the statistics of time when the server was locked while awaiting the input.
    [<CompiledName ("InputWaitTime")>]
    val inputWaitTime: server:Server<'state, 'a, 'b> -> Eventive<SamplingStats<float>>

    /// A signal triggered when the input wait time statistics changes.
    [<CompiledName ("InputWaitTimeChanged_")>]
    val inputWaitTimeChanged_: server:Server<'state, 'a, 'b> -> Signal<unit>

    /// A signal triggered when the input wait time statistics changes.
    [<CompiledName ("InputWaitTimeChanged")>]
    val inputWaitTimeChanged: server:Server<'state, 'a, 'b> -> Signal<SamplingStats<float>>

    /// Returns the statistics of time spent by the server while processing the tasks.
    [<CompiledName ("ProcessingTime")>]
    val processingTime: server:Server<'state, 'a, 'b> -> Eventive<SamplingStats<float>>

    /// A signal triggered when the processing time statistics changes.
    [<CompiledName ("ProcessingTimeChanged_")>]
    val processingTimeChanged_: server:Server<'state, 'a, 'b> -> Signal<unit>

    /// A signal triggered when the processing time statistics changes.
    [<CompiledName ("ProcessingTimeChanged")>]
    val processingTimeChanged: server:Server<'state, 'a, 'b> -> Signal<SamplingStats<float>>

    /// Returns the statistics of time when the server was locked while trying to deliver the output.
    [<CompiledName ("OutputWaitTime")>]
    val outputWaitTime: server:Server<'state, 'a, 'b> -> Eventive<SamplingStats<float>>

    /// A signal triggered when the output wait time statistics changes.
    [<CompiledName ("OutputWaitTimeChanged_")>]
    val outputWaitTimeChanged_: server:Server<'state, 'a, 'b> -> Signal<unit>

    /// A signal triggered when the output wait time statistics changes.
    [<CompiledName ("OutputWaitTimeChanged")>]
    val outputWaitTimeChanged: server:Server<'state, 'a, 'b> -> Signal<SamplingStats<float>>

    /// Returns the statistics of time spent by the server while its processing was preempted.
    [<CompiledName ("PreemptionTime")>]
    val preemptionTime: server:Server<'state, 'a, 'b> -> Eventive<SamplingStats<float>>

    /// A signal triggered when the preemption time statistics changes.
    [<CompiledName ("PreemptionTimeChanged_")>]
    val preemptionTimeChanged_: server:Server<'state, 'a, 'b> -> Signal<unit>

    /// A signal triggered when the preemption time statistics changes.
    [<CompiledName ("PreemptionTimeChanged")>]
    val preemptionTimeChanged: server:Server<'state, 'a, 'b> -> Signal<SamplingStats<float>>

    /// It returns a factor changing from 0 to 1, which estimates how long the server was awaiting for the next input task.
    [<CompiledName ("InputWaitFactor")>]
    val inputWaitFactor: server:Server<'state, 'a, 'b> -> Eventive<float>

    /// A signal triggered when the input wait factor changes.
    [<CompiledName ("InputWaitFactorChanged_")>]
    val inputWaitFactorChanged_: server:Server<'state, 'a, 'b> -> Signal<unit>

    /// A signal triggered when the input wait factor changes.
    [<CompiledName ("InputWaitFactorChanged")>]
    val inputWaitFactorChanged: server:Server<'state, 'a, 'b> -> Signal<float>

    /// It returns a factor changing from 0 to 1, which estimates how long the server was busy with direct processing its tasks.
    [<CompiledName ("ProcessingFactor")>]
    val processingFactor: server:Server<'state, 'a, 'b> -> Eventive<float>

    /// A signal triggered when the processing factor changes.
    [<CompiledName ("ProcessingFactorChanged_")>]
    val processingFactorChanged_: Server<'state, 'a, 'b> -> Signal<unit>

    /// A signal triggered when the processing factor changes.
    [<CompiledName ("ProcessingFactorChanged")>]
    val processingFactorChanged: server:Server<'state, 'a, 'b> -> Signal<float>

    /// Returns a factor changing from 0 to 1, which estimates how long the server was locked trying to deliver the output after the task is finished.
    [<CompiledName ("OutputWaitFactor")>]
    val outputWaitFactor: server:Server<'state, 'a, 'b> -> Eventive<float>

    /// A signal triggered when the output wait factor changes.
    [<CompiledName ("OutputWaitFactorChanged_")>]
    val outputWaitFactorChanged_: server:Server<'state, 'a, 'b> -> Signal<unit>

    /// A signal triggered when the output wait factor changes.
    [<CompiledName ("OutputWaitFactorChanged")>]
    val outputWaitFactorChanged: server:Server<'state, 'a, 'b> -> Signal<float>

    /// It returns a factor changing from 0 to 1, which estimates how long the server was preempted while trying to process its tasks.
    [<CompiledName ("PreemptionFactor")>]
    val preemptionFactor: server:Server<'state, 'a, 'b> -> Eventive<float>

    /// A signal triggered when the preemption factor changes.
    [<CompiledName ("PreemptionFactorChanged_")>]
    val preemptionFactorChanged_: Server<'state, 'a, 'b> -> Signal<unit>

    /// A signal triggered when the preemption factor changes.
    [<CompiledName ("PreemptionFactorChanged")>]
    val preemptionFactorChanged: server:Server<'state, 'a, 'b> -> Signal<float>

    /// Triggers whenever any property of the server changes.
    [<CompiledName ("Changed_")>]
    val changed_: server:Server<'state, 'a, 'b> -> Signal<unit>
    
    /// Creates a new server that holds the process for a random time interval distributed uniformly with the specified minimum and maximum, when processing every input element.
    [<CompiledName ("CreateRandomUniformPreemptible")>]
    val createRandomUniformPreemptible: preemptible:bool -> minimum:float -> maximum:float -> Simulation<Server<unit, 'a, 'a>>
    
    /// Creates a new non-preemptible server that holds the process for a random time interval distributed uniformly with the specified minimum and maximum, when processing every input element.
    [<CompiledName ("CreateRandomUniform")>]
    val createRandomUniform: minimum:float -> maximum:float -> Simulation<Server<unit, 'a, 'a>>
    
    /// Creates a new server that holds the process for an integer random time interval distributed uniformly with the specified minimum and maximum, when processing every input element.
    [<CompiledName ("CreateRandomUniformIntPreemptible")>]
    val createRandomUniformIntPreemptible: preemptible:bool -> minimum:int -> maximum:int -> Simulation<Server<unit, 'a, 'a>>
    
    /// Creates a new non-preemptible server that holds the process for an integer random time interval distributed uniformly with the specified minimum and maximum, when processing every input element.
    [<CompiledName ("CreateRandomUniformInt")>]
    val createRandomUniformInt: minimum:int -> maximum:int -> Simulation<Server<unit, 'a, 'a>>
    
    /// Creates a new server that holds the process for a random time interval distributed normally with the specified mean and deviation, when processing every input element.
    [<CompiledName ("CreateRandomNormalPreemptible")>]
    val createRandomNormalPreemptible: preemptible:bool -> mean:float -> deviation:float -> Simulation<Server<unit, 'a, 'a>>
    
    /// Creates a new non-preemptible server that holds the process for a random time interval distributed normally with the specified mean and deviation, when processing every input element.
    [<CompiledName ("CreateRandomNormal")>]
    val createRandomNormal: mean:float -> deviation:float -> Simulation<Server<unit, 'a, 'a>>
    
    /// Creates a new server that holds the process for a random time interval distributed exponentially with the specified mean (the reciprocal of the rate), when processing every input element.
    [<CompiledName ("CreateRandomExponentialPreemptible")>]
    val createRandomExponentialPreemptible: preemptible:bool -> mean:float -> Simulation<Server<unit, 'a, 'a>>
    
    /// Creates a new non-preemptible server that holds the process for a random time interval distributed exponentially with the specified mean (the reciprocal of the rate), when processing every input element.
    [<CompiledName ("CreateRandomExponential")>]
    val createRandomExponential: mean:float -> Simulation<Server<unit, 'a, 'a>>
    
    /// Creates a new server that holds the process for a random time interval having the Erlang distribution with the specified scale (the reciprocal of the rate) and integer shape, when processing every input element.
    [<CompiledName ("CreateRandomErlangPreemptible")>]
    val createRandomErlangPreemptible: preemptible:bool -> beta:float -> m:int -> Simulation<Server<unit, 'a, 'a>>
    
    /// Creates a new non-preemptible server that holds the process for a random time interval having the Erlang distribution with the specified scale (the reciprocal of the rate) and integer shape, when processing every input element.
    [<CompiledName ("CreateRandomErlang")>]
    val createRandomErlang: beta:float -> m:int -> Simulation<Server<unit, 'a, 'a>>
    
    /// Creates a new server that holds the process for a random time interval having the Poisson distribution with the specified mean, when processing every input element.
    [<CompiledName ("CreateRandomPoissonPreemptible")>]
    val createRandomPoissonPreemptible: preemptible:bool -> mean:float -> Simulation<Server<unit, 'a, 'a>>
    
    /// Creates a new non-preemptible server that holds the process for a random time interval having the Poisson distribution with the specified mean, when processing every input element.
    [<CompiledName ("CreateRandomPoisson")>]
    val createRandomPoisson: mean:float -> Simulation<Server<unit, 'a, 'a>>
    
    /// Creates a new server that holds the process for a random time interval distributed binomially with the specified probability and number of trials, when processing every input element.
    [<CompiledName ("CreateRandomBinomialPreemptible")>]
    val createRandomBinomialPreemptible: preemptible:bool -> prob:float -> trials:int -> Simulation<Server<unit, 'a, 'a>>
    
    /// Creates a new non-preemptible server that holds the process for a random time interval distributed binomially with the specified probability and number of trials, when processing every input element.
    [<CompiledName ("CreateRandomBinomial")>]
    val createRandomBinomial: prob:float -> trials:int -> Simulation<Server<unit, 'a, 'a>>
