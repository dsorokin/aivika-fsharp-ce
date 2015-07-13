
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

type Processor<'a, 'b> = Stream<'a> -> Stream<'b>

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Processor =

    [<CompiledName ("Run")>]
    let run xs (p: Processor<_, _>) = p xs

    [<CompiledName ("UsingId")>]
    let usingId pid p =
        fun (xs: Stream<_>) -> p xs |> Stream.usingId pid 

    [<CompiledName ("Never")>]
    let never =
        fun (xs: Stream<'a>) -> Stream.never<'b>

    [<CompiledName ("Arr")>]
    let arr f = Stream.map f
    
    [<CompiledName ("ArrC")>]
    let arrc f = Stream.mapc f

    [<CompiledName ("Accum")>]
    let accum f acc = Stream.accum f acc

    [<CompiledName ("Within")>]
    let within m =
        Stream.mapc (fun a ->
            proc {
                do! m
                return a
            })

    [<CompiledName ("Delay")>]
    let delay x =
        fun xs -> Stream (proc.Return (StreamCons (x, xs)))

    [<CompiledName ("ParQueueing")>]
    let parQueueing (istrat: #IQueueStrategy) (ostrat: #IQueueStrategy) processors = 
        let n = List.length processors
        fun xs ->
            let input = Stream.splitQueueing istrat n xs
            let ys = List.zip input processors 
                        |> List.map (fun (input, p) -> p input)
            let output = Stream.mergeQueueing ostrat ys
            output 

    [<CompiledName ("ParPrioritisingOutput")>]
    let parPrioritisingOutput (istrat: #IQueueStrategy) (ostrat: #IQueueStrategy) processors = 
        let n = List.length processors
        fun xs ->
            let input = Stream.splitQueueing istrat n xs
            let ys = List.zip input processors 
                        |> List.map (fun (input, p) -> p input)
            let output = Stream.mergePrioritising ostrat ys
            output 

    [<CompiledName ("ParPrioritisingInput")>]
    let parPrioritisingInput (istrat: #IQueueStrategy) (ostrat: #IQueueStrategy) processors = 
        fun xs ->
            let input = Stream.splitPrioritising istrat (List.map fst processors) xs
            let ys = List.zip input processors 
                        |> List.map (fun (input, (_, p)) -> p input)
            let output = Stream.mergeQueueing ostrat ys
            output 

    [<CompiledName ("ParPrioritisingInputOutput")>]
    let parPrioritisingInputOutput (istrat: #IQueueStrategy) (ostrat: #IQueueStrategy) processors = 
        fun xs ->
            let input = Stream.splitPrioritising istrat (List.map fst processors) xs
            let ys = List.zip input processors 
                        |> List.map (fun (input, (_, p)) -> p input)
            let output = Stream.mergePrioritising ostrat ys
            output 

    [<CompiledName ("Par")>]
    let par processors = 
        parQueueing QueueStrategy.FCFS QueueStrategy.FCFS processors

    [<CompiledName ("Seq")>]
    let seq processors =
        match processors with
        | [] -> never
        | _  -> processors |> List.reduce (fun p1 p2 -> p1 >> Stream.prefetch >> p2)

    [<CompiledName ("Buffer")>]
    let buffer consumer producer =
        fun (xs: Stream<_>) -> Stream (proc {
            do! consumer xs |> Proc.spawn
            return! invokeStream producer
        })

    [<CompiledName ("BufferLoop")>]
    let bufferLoop consumer producer (cond: Processor<_, _>) (body: Processor<_, _>) =
        fun (xs: Stream<_>) -> Stream (proc {
            let (reverted, output) = cond producer |> Stream.partitionChoice
            do! consumer xs (body reverted) |> Proc.spawn
            return! invokeStream output
        })

    [<CompiledName ("Queue")>]
    let queue enqueue dequeue =
        buffer (Stream.consume enqueue)
               (Stream.repeat dequeue)

    [<CompiledName ("QueueLoopMerging")>]
    let queueLoopMerging merge enqueue dequeue cond body =
        bufferLoop (fun xs ys -> Stream.consume enqueue (merge xs ys))
                   (Stream.repeat dequeue)
                   cond body

    [<CompiledName ("QueueLoopSeq")>]
    let queueLoopSeq enqueue dequeue cond body =
        queueLoopMerging Stream.merge2 enqueue dequeue cond body

    [<CompiledName ("QueueLoopPar")>]
    let queueLoopPar enqueue dequeue cond body =
        bufferLoop (fun xs ys ->
                        proc {
                            do! Stream.consume enqueue xs
                                    |> Proc.spawn
                            do! Stream.consume enqueue ys
                                    |> Proc.spawn
                        })
                    (Stream.repeat dequeue)
                    cond body

    [<CompiledName ("OfSignaling")>]
    let ofSignaling f =
        fun xs -> Stream (proc {
            let! sa = Stream.toSignal xs
            let! sb = Stream.ofSignal (f sa)
            return! invokeStream sb
        })

    [<CompiledName ("ToSignaling")>]
    let toSignaling m sa =
        proc {
            let! xs = Stream.ofSignal sa
            let ys = m xs
            return! Stream.toSignal ys
        }

    [<CompiledName ("RandomUniform")>]
    let randomUniform minimum maximum =
        Proc.randomUniform_ minimum maximum |> within
    
    [<CompiledName ("RandomUniformInt")>]
    let randomUniformInt minimum maximum =
        Proc.randomUniformInt_ minimum maximum |> within

    [<CompiledName ("RandomTriangular")>]
    let randomTriangular minimum median maximum =
        Proc.randomTriangular_ minimum median maximum |> within
    
    [<CompiledName ("RandomNormal")>]
    let randomNormal mu nu =
        Proc.randomNormal_ mu nu |> within
    
    [<CompiledName ("RandomExponential")>]
    let randomExponential mu =
        Proc.randomExponential_ mu |> within
    
    [<CompiledName ("RandomErlang")>]
    let randomErlang beta m =
        Proc.randomErlang_ beta m |> within
    
    [<CompiledName ("RandomPoisson")>]
    let randomPoisson mu =
        Proc.randomPoisson_ mu |> within
    
    [<CompiledName ("RandomBinomial")>]
    let randomBinomial prob trials =
        Proc.randomBinomial_ prob trials |> within

    [<CompiledName ("Trace")>]
    let trace request response nil p =
        fun (xs: Stream<_>) -> Stream.trace request response nil (p xs)  
