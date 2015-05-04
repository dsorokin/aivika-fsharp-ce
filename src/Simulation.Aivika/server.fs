
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

type Server<'state, 'a, 'b> =
    { InitState: 'state;
      mutable State: 'state;
      Accum: 'state -> 'a -> Proc<'state * 'b>;
      AccumPreemptible: bool;
      mutable TotalInputWaitTime: float;
      mutable TotalProcessingTime: float;
      mutable TotalOutputWaitTime: float;
      mutable TotalPreemptionTime: float;
      mutable InputWaitTime: SamplingStats<float>;
      mutable ProcessingTime: SamplingStats<float>;
      mutable OutputWaitTime: SamplingStats<float>;
      mutable PreemptionTime: SamplingStats<float>;
      InputReceivedSource: SignalSource<'a>;
      TaskProcessedSource: SignalSource<'a * 'b>;
      TaskPreemptionBeginningSource: SignalSource<'a>;
      TaskPreemptionEndingSource: SignalSource<'a>;
      OutputProvidedSource: SignalSource<'a * 'b> }

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Server =

    [<CompiledName ("CreateAccumPreemptible")>]
    let createAccumPreemptible preemptible f st =
        simulation {
            let! s1 = SignalSource.create
            let! s2 = SignalSource.create
            let! s3 = SignalSource.create
            let! s4 = SignalSource.create
            let! s5 = SignalSource.create
            return { 
                InitState = st;
                State = st;
                Accum = f;
                AccumPreemptible = preemptible;
                TotalInputWaitTime = 0.0;
                TotalProcessingTime = 0.0;
                TotalOutputWaitTime = 0.0;
                TotalPreemptionTime = 0.0;
                InputWaitTime = SamplingStats.emptyFloats;
                ProcessingTime = SamplingStats.emptyFloats;
                OutputWaitTime = SamplingStats.emptyFloats;
                PreemptionTime = SamplingStats.emptyFloats;
                InputReceivedSource = s1;
                TaskProcessedSource = s2;
                TaskPreemptionBeginningSource = s3;
                TaskPreemptionEndingSource = s4;
                OutputProvidedSource = s5 
            }
        }

    [<CompiledName ("CreatePreemptible")>]
    let createPreemptible preemptible f =
        let g () a = 
            proc {
                let! b = f a
                return ((), b)
            }
        in createAccumPreemptible preemptible g ()

    [<CompiledName ("CreateAccum")>]
    let createAccum f st = createAccumPreemptible false f st

    [<CompiledName ("Create")>]
    let create f = createPreemptible false f

    let private accumPreempting (server: Server<_, _, _>) st a =
        proc {
            let! pid = Proc.id
            let! t1  = Dynamics.time |> Dynamics.lift

            let rs = ref 0.0
            let r1 = ref t1

            let handle1 () = eventive {
                    
                    let! t1 = Dynamics.time |> Dynamics.lift
                    r1 := t1

                    do! server.TaskPreemptionBeginningSource
                            |> SignalSource.trigger a
                }

            let handle2 () = eventive {
                    let  t1 = !r1
                    let! t2 = Dynamics.time |> Dynamics.lift
                    let  dt = t2 - t1

                    rs := !rs + dt

                    server.TotalPreemptionTime <-
                        server.TotalPreemptionTime + dt
                    server.PreemptionTime <-
                        server.PreemptionTime 
                            |> SamplingStats.add dt

                    do! server.TaskPreemptionEndingSource 
                            |> SignalSource.trigger a
                }

            use! h1 = 
                Proc.preemptionBeginning pid 
                    |> Signal.subscribe handle1
                    |> Eventive.lift

            use! h2 = 
                Proc.preemptionEnding pid 
                    |> Signal.subscribe handle2
                    |> Eventive.lift

            let! (st', b) = server.Accum st a

            return (st', b, !rs)
        }

    [<CompiledName ("Processor")>]
    let processor (server: Server<_, _, _>) =
        let rec loop st r xs =
            Stream (proc {

                let! t0 = Dynamics.time |> Dynamics.lift

                match r with
                | None -> ()
                | Some (t', a', b') ->

                    // inform about delivering the output

                    server.TotalOutputWaitTime <- 
                        server.TotalOutputWaitTime + (t0 - t')
                    server.OutputWaitTime <- 
                        server.OutputWaitTime 
                            |> SamplingStats.add (t0 - t')

                    do! server.OutputProvidedSource 
                            |> SignalSource.trigger (a', b')
                            |> Eventive.lift

                // get the input

                let! x = invokeStream xs

                match x with
                | StreamNil ->
                    return StreamNil
                | StreamCons (a, xs') -> 
                  
                    let! t1 = Dynamics.time |> Dynamics.lift

                    server.TotalInputWaitTime <-
                        server.TotalInputWaitTime + (t1 - t0)
                    server.InputWaitTime <-
                        server.InputWaitTime 
                            |> SamplingStats.add (t1 - t0)

                    do! server.InputReceivedSource 
                        |> SignalSource.trigger a
                        |> Eventive.lift

                    // take the input to provide an output

                    let! (st', b, dt) = 
                        if server.AccumPreemptible 
                        then accumPreempting server st a
                        else proc.Bind (server.Accum st a, fun (st', b) -> proc.Return (st', b, 0.0))

                    let! t2 = Dynamics.time |> Dynamics.lift

                    server.State <- st'
                    server.TotalProcessingTime <-
                        server.TotalProcessingTime + (t2 - t1 - dt)
                    server.ProcessingTime <-
                        server.ProcessingTime 
                            |> SamplingStats.add (t2 - t1 - dt)

                    do! server.TaskProcessedSource 
                            |> SignalSource.trigger (a, b)
                            |> Eventive.lift

                    return StreamCons (b, loop st' (Some (t2, a, b)) xs')
            })
        in fun xs -> loop server.InitState None xs

    [<CompiledName ("InputReceived")>]
    let inputReceived (server: Server<_, _, _>) =
        server.InputReceivedSource |> SignalSource.publish

    [<CompiledName ("TaskProcessed")>]
    let taskProcessed (server: Server<_, _, _>) =
        server.TaskProcessedSource |> SignalSource.publish

    [<CompiledName ("TaskPreemptionBeginning")>]
    let taskPreemptionBeginning (server: Server<_, _, _>) =
        server.TaskPreemptionBeginningSource |> SignalSource.publish

    [<CompiledName ("TaskPreemptionEnding")>]
    let taskPreemptionEnding (server: Server<_, _, _>) =
        server.TaskPreemptionEndingSource |> SignalSource.publish

    [<CompiledName ("OutputProvided")>]
    let outputProvided (server: Server<_, _, _>) =
        server.OutputProvidedSource |> SignalSource.publish

    [<CompiledName ("InitState")>]
    let initState (server: Server<_, _, _>) = server.InitState

    [<CompiledName ("State")>]
    let state (server: Server<_, _, _>) =
        Eventive (fun p -> server.State)

    [<CompiledName ("StateChanged_")>]
    let stateChanged_ (server: Server<_, _, _>) =
        taskProcessed server |> Signal.map (fun x -> ())

    [<CompiledName ("StateChanged")>]
    let stateChanged (server: Server<_, _, _>) =
        taskProcessed server |> Signal.map (fun x -> server.State)

    [<CompiledName ("TotalInputWaitTime")>]
    let totalInputWaitTime (server: Server<_, _, _>) =
        Eventive (fun p -> server.TotalInputWaitTime)

    [<CompiledName ("TotalInputWaitTimeChanged_")>]
    let totalInputWaitTimeChanged_ (server: Server<_, _, _>) =
        inputReceived server |> Signal.map (fun x -> ())

    [<CompiledName ("TotalInputWaitTimeChanged")>]
    let totalInputWaitTimeChanged (server: Server<_, _, _>) =
        inputReceived server |> Signal.map (fun x -> server.TotalInputWaitTime)

    [<CompiledName ("TotalProcessingTime")>]
    let totalProcessingTime (server: Server<'state, 'a, 'b>) =
        Eventive (fun p -> server.TotalProcessingTime)

    [<CompiledName ("TotalProcessingTimeChanged_")>]
    let totalProcessingTimeChanged_ (server: Server<_, _, _>) =
        taskProcessed server |> Signal.map (fun x -> ())

    [<CompiledName ("TotalProcessingTimeChanged")>]
    let totalProcessingTimeChanged (server: Server<_, _, _>) =
        taskProcessed server |> Signal.map (fun x -> server.TotalProcessingTime)

    [<CompiledName ("TotalOutputWaitTime")>]
    let totalOutputWaitTime (server: Server<_, _, _>) =
        Eventive (fun p -> server.TotalOutputWaitTime)

    [<CompiledName ("TotalOutputWaitTimeChanged_")>]
    let totalOutputWaitTimeChanged_ (server: Server<_, _, _>) =
        outputProvided server |> Signal.map (fun x -> ())

    [<CompiledName ("TotalOutputWaitTimeChanged")>]
    let totalOutputWaitTimeChanged (server: Server<_, _, _>) =
        outputProvided server |> Signal.map (fun x -> server.TotalOutputWaitTime)

    [<CompiledName ("TotalPreemptionTime")>]
    let totalPreemptionTime (server: Server<'state, 'a, 'b>) =
        Eventive (fun p -> server.TotalPreemptionTime)

    [<CompiledName ("TotalPreemptionTimeChanged_")>]
    let totalPreemptionTimeChanged_ (server: Server<_, _, _>) =
        taskPreemptionEnding server |> Signal.map (fun x -> ())

    [<CompiledName ("TotalPreemptionTimeChanged")>]
    let totalPreemptionTimeChanged (server: Server<_, _, _>) =
        taskPreemptionEnding server |> Signal.map (fun x -> server.TotalPreemptionTime)

    [<CompiledName ("InputWaitTime")>]
    let inputWaitTime (server: Server<_, _, _>) =
        Eventive (fun p -> server.InputWaitTime)

    [<CompiledName ("InputWaitTimeChanged_")>]
    let inputWaitTimeChanged_ (server: Server<_, _, _>) =
        inputReceived server |> Signal.map (fun x -> ())

    [<CompiledName ("InputWaitTimeChanged")>]
    let inputWaitTimeChanged (server: Server<_, _, _>) =
        inputReceived server |> Signal.map (fun x -> server.InputWaitTime)

    [<CompiledName ("ProcessingTime")>]
    let processingTime (server: Server<_, _, _>) =
        Eventive (fun p -> server.ProcessingTime)

    [<CompiledName ("ProcessingTimeChanged_")>]
    let processingTimeChanged_ (server: Server<_, _, _>) =
        taskProcessed server |> Signal.map (fun x -> ())

    [<CompiledName ("ProcessingTimeChanged")>]
    let processingTimeChanged (server: Server<_, _, _>) =
        taskProcessed server |> Signal.map (fun x -> server.ProcessingTime)

    [<CompiledName ("OutputWaitTime")>]
    let outputWaitTime (server: Server<_, _, _>) =
        Eventive (fun p -> server.OutputWaitTime)

    [<CompiledName ("OutputWaitTimeChanged_")>]
    let outputWaitTimeChanged_ (server: Server<_, _, _>) =
        outputProvided server |> Signal.map (fun x -> ())

    [<CompiledName ("OutputWaitTimeChanged")>]
    let outputWaitTimeChanged (server: Server<_, _, _>) =
        outputProvided server |> Signal.map (fun x -> server.OutputWaitTime)

    [<CompiledName ("PreemptionTime")>]
    let preemptionTime (server: Server<_, _, _>) =
        Eventive (fun p -> server.PreemptionTime)

    [<CompiledName ("PreemptionTimeChanged_")>]
    let preemptionTimeChanged_ (server: Server<_, _, _>) =
        taskPreemptionEnding server |> Signal.map (fun x -> ())

    [<CompiledName ("PreemptionTimeChanged")>]
    let preemptionTimeChanged (server: Server<_, _, _>) =
        taskPreemptionEnding server |> Signal.map (fun x -> server.PreemptionTime)

    [<CompiledName ("InputWaitFactor")>]
    let inputWaitFactor (server: Server<_, _, _>) =
        Eventive (fun p ->
            let x1 = server.TotalInputWaitTime
            let x2 = server.TotalProcessingTime
            let x3 = server.TotalOutputWaitTime
            let x4 = server.TotalPreemptionTime
            x1 / (x1 + x2 + x3 + x4))

    [<CompiledName ("InputWaitFactorChanged_")>]
    let inputWaitFactorChanged_ (server: Server<_, _, _>) =
        [totalInputWaitTimeChanged_ server;
         totalProcessingTimeChanged_ server;
         totalOutputWaitTimeChanged_ server;
         totalPreemptionTimeChanged_ server;] 
            |> Signal.concat

    [<CompiledName ("InputWaitFactorChanged")>]
    let inputWaitFactorChanged (server: Server<_, _, _>) =
        inputWaitFactorChanged_ server |> Signal.mapc (fun x -> inputWaitFactor server)

    [<CompiledName ("ProcessingFactor")>]
    let processingFactor (server: Server<_, _, _>) =
        Eventive (fun p ->
            let x1 = server.TotalInputWaitTime
            let x2 = server.TotalProcessingTime
            let x3 = server.TotalOutputWaitTime
            let x4 = server.TotalPreemptionTime
            x2 / (x1 + x2 + x3 + x4))

    [<CompiledName ("ProcessingFactorChanged_")>]
    let processingFactorChanged_ (server: Server<_, _, _>) =
        [totalInputWaitTimeChanged_ server;
         totalProcessingTimeChanged_ server;
         totalOutputWaitTimeChanged_ server;
         totalPreemptionTimeChanged_ server] 
            |> Signal.concat

    [<CompiledName ("ProcessingFactorChanged")>]
    let processingFactorChanged (server: Server<_, _, _>) =
        processingFactorChanged_ server |> Signal.mapc (fun x -> processingFactor server)

    [<CompiledName ("OutputWaitFactor")>]
    let outputWaitFactor (server: Server<_, _, _>) =
        Eventive (fun p ->
            let x1 = server.TotalInputWaitTime
            let x2 = server.TotalProcessingTime
            let x3 = server.TotalOutputWaitTime
            let x4 = server.TotalPreemptionTime
            x3 / (x1 + x2 + x3 + x4))

    [<CompiledName ("OutputWaitFactorChanged_")>]
    let outputWaitFactorChanged_ (server: Server<_, _, _>) =
        [totalInputWaitTimeChanged_ server;
         totalProcessingTimeChanged_ server;
         totalOutputWaitTimeChanged_ server;
         totalPreemptionTimeChanged_ server] 
            |> Signal.concat

    [<CompiledName ("OutputWaitFactorChanged")>]
    let outputWaitFactorChanged (server: Server<_, _, _>) =
        outputWaitFactorChanged_ server |> Signal.mapc (fun x -> outputWaitFactor server)

    [<CompiledName ("PreemptionFactor")>]
    let preemptionFactor (server: Server<_, _, _>) =
        Eventive (fun p ->
            let x1 = server.TotalInputWaitTime
            let x2 = server.TotalProcessingTime
            let x3 = server.TotalOutputWaitTime
            let x4 = server.TotalPreemptionTime
            x4 / (x1 + x2 + x3 + x4))

    [<CompiledName ("PreemptionFactorChanged_")>]
    let preemptionFactorChanged_ (server: Server<_, _, _>) =
        [totalInputWaitTimeChanged_ server;
         totalProcessingTimeChanged_ server;
         totalOutputWaitTimeChanged_ server;
         totalPreemptionTimeChanged_ server] 
            |> Signal.concat

    [<CompiledName ("PreemptionFactorChanged")>]
    let preemptionFactorChanged (server: Server<_, _, _>) =
        preemptionFactorChanged_ server |> Signal.mapc (fun x -> preemptionFactor server)

    [<CompiledName ("Changed_")>]
    let changed_ (server: Server<_, _, _>) =
        [totalInputWaitTimeChanged_ server;
         totalProcessingTimeChanged_ server;
         totalOutputWaitTimeChanged_ server;
         totalPreemptionTimeChanged_ server] 
            |> Signal.concat
    
    [<CompiledName ("CreateRandomUniformPreemptible")>]
    let createRandomUniformPreemptible preemptible minimum maximum =
        createPreemptible preemptible (fun a -> proc {
            do! Proc.randomUniform_ minimum maximum
            return a
        })
    
    [<CompiledName ("CreateRandomUniform")>]
    let createRandomUniform minimum maximum =
        createRandomUniformPreemptible false minimum maximum
    
    [<CompiledName ("CreateRandomUniformIntPreemptible")>]
    let createRandomUniformIntPreemptible preemptible minimum maximum = 
        createPreemptible preemptible (fun a -> proc {
            do! Proc.randomUniformInt_ minimum maximum
            return a
        })
    
    [<CompiledName ("CreateRandomUniformInt")>]
    let createRandomUniformInt minimum maximum =
        createRandomUniformIntPreemptible false minimum maximum
    
    [<CompiledName ("CreateRandomNormalPreemptible")>]
    let createRandomNormalPreemptible preemptible mu nu =
        createPreemptible preemptible (fun a -> proc {
            do! Proc.randomNormal_ mu nu
            return a
        })
    
    [<CompiledName ("CreateRandomNormal")>]
    let createRandomNormal mu nu =
        createRandomNormalPreemptible false mu nu
    
    [<CompiledName ("CreateRandomExponentialPreemptible")>]
    let createRandomExponentialPreemptible preemptible mu =
        createPreemptible preemptible (fun a -> proc {
            do! Proc.randomExponential_ mu
            return a
        })
    
    [<CompiledName ("CreateRandomExponential")>]
    let createRandomExponential mu =
        createRandomExponentialPreemptible false mu
    
    [<CompiledName ("CreateRandomErlangPreemptible")>]
    let createRandomErlangPreemptible preemptible beta m =
        createPreemptible preemptible (fun a -> proc {
            do! Proc.randomErlang_ beta m
            return a
        })
    
    [<CompiledName ("CreateRandomErlang")>]
    let createRandomErlang beta m =
        createRandomErlangPreemptible false beta m
    
    [<CompiledName ("CreateRandomPoissonPreemptible")>]
    let createRandomPoissonPreemptible preemptible mu =
        createPreemptible preemptible (fun a -> proc {
            do! Proc.randomPoisson_ mu
            return a
        })
    
    [<CompiledName ("CreateRandomPoisson")>]
    let createRandomPoisson mu =
        createRandomPoissonPreemptible false mu
    
    [<CompiledName ("CreateRandomBinomialPreemptible")>]
    let createRandomBinomialPreemptible preemptible prob trials =
        createPreemptible preemptible (fun a -> proc {
            do! Proc.randomBinomial_ prob trials
            return a
        })
    
    [<CompiledName ("CreateRandomBinomial")>]
    let createRandomBinomial prob trials = 
        createRandomBinomialPreemptible false prob trials
