
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

[<Sealed; NoComparison; NoEquality>]
type Dynamics<'a> (f: Point -> 'a) =

    member internal x.Fun = f
    
    static member From (m: Parameter<'a>) = 
        Dynamics (fun p -> invokeParameter p.Run m)
    
    static member From (m: Simulation<'a>) = 
        Dynamics (fun p -> invokeSimulation p.Run m)

    static member From (m: Dynamics<'a>) = m

[<AutoOpen>]
module internal DynamicsInvoke =

    let inline invokeDynamics p (m: Dynamics<'a>) = m.Fun p
    
module DynamicsBuilderImpl =

    let inline returnD a =
        Dynamics (fun p -> a)

    let inline bindD m k =
        Dynamics (fun p ->
            let a = invokeDynamics p m
            let m' = k a
            invokeDynamics p m')

    //  letD a k = bindD (returnD a) k
    let inline letD a k =
        Dynamics (fun p ->
            let m' = k a
            invokeDynamics p m')

    //  delayD k = bindD (returnD ()) k
    let inline delayD k =
        Dynamics (fun p ->
            let m' = k ()
            invokeDynamics p m')
            
    //  combineD m1 m2 = bindD m1 (fun () -> m2)
    let inline combineD m1 m2 =
        Dynamics (fun p ->
            invokeDynamics p m1
            invokeDynamics p m2)
            
    let zeroD = Dynamics (fun p -> ())
    
    let inline forD es k  =
        Dynamics (fun p ->
            for e in es do
                invokeDynamics p (k e))
                
    let inline whileD pred m =
        Dynamics (fun p ->
            while pred () do
                invokeDynamics p m)
                
    let inline usingD (a: 'a when 'a :> IDisposable) k =
        Dynamics (fun p ->
            try
                invokeDynamics p (k a)
            finally
                a.Dispose ())
                
    let inline tryFinallyD m f =
        Dynamics (fun p ->
            try
                invokeDynamics p m
            finally
                f ())
                
    let inline tryWithD m k =
        Dynamics (fun p ->
            try
                invokeDynamics p m
            with
            | e -> invokeDynamics p (k e))

open DynamicsBuilderImpl

[<Sealed>]
type DynamicsBuilder() =

    member d.Return (a)  = returnD a
    member d.ReturnFrom (m: Dynamics<_>) = m
    member d.Bind (m: Dynamics<_>, k) = bindD m k
    member d.Delay (k)   = delayD k
    member d.Zero () = zeroD
    member d.Combine (m1: Dynamics<_>, m2) = combineD m1 m2
    member d.For (es, k) = forD es k
    member d.While (p, m) = whileD p m
    member d.Using (a, k) = usingD a k
    member d.TryFinally (m, f) = tryFinallyD m f
    member d.TryWith (m, k) = tryWithD m k

[<AutoOpen>]
module DynamicsWorkflow =
    let dynamics = new DynamicsBuilder()

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Dynamics =

    [<CompiledName ("RunInStartTime")>]
    let runInStartTime m =
        Simulation (fun r -> invokeDynamics r.IntegStartPoint m)

    [<CompiledName ("RunInStopTime")>]
    let runInStopTime m =
        Simulation (fun r -> invokeDynamics r.IntegStopPoint m)

    [<CompiledName ("RunInIntegTimes")>]
    let runInIntegTimes m =
        Simulation (fun r -> seq {
        
            for p in r.IntegPoints do
                yield invokeDynamics p m
        })

    [<CompiledName ("RunInTimes")>]
    let runInTimes ts m =
        Simulation (fun r -> seq {
        
            for t in ts do

                let p = Run.point r t
                yield invokeDynamics p m
        })

    [<CompiledName ("Lift")>]
    let inline lift (x: Dynamics<'a>) = (^m: (static member From: Dynamics<'a> -> ^m) (x))

    [<CompiledName ("Concat")>]
    let concat mm = Dynamics (fun p ->
        let m = invokeDynamics p mm
        invokeDynamics p m)
    
    [<CompiledName ("Map")>]
    let map f m = Dynamics (fun p ->
        let a = invokeDynamics p m
        in f a)
    
    [<CompiledName ("Ap")>]
    let ap mf m = Dynamics (fun p ->
        let f = invokeDynamics p mf
        let a = invokeDynamics p m
        in f a)
    
    [<CompiledName ("Lift2")>]
    let lift2 f m1 m2 = Dynamics (fun p ->
        let a1 = invokeDynamics p m1
        let a2 = invokeDynamics p m2
        in f a1 a2)
    
    [<CompiledName ("Lift3")>]
    let lift3 f m1 m2 m3 = Dynamics (fun p ->
        let a1 = invokeDynamics p m1
        let a2 = invokeDynamics p m2
        let a3 = invokeDynamics p m3
        in f a1 a2 a3)
    
    [<CompiledName ("Lift4")>]
    let lift4 f m1 m2 m3 m4 = Dynamics (fun p ->
        let a1 = invokeDynamics p m1
        let a2 = invokeDynamics p m2
        let a3 = invokeDynamics p m3
        let a4 = invokeDynamics p m4
        in f a1 a2 a3 a4)
    
    [<CompiledName ("Lift5")>]
    let lift5 f m1 m2 m3 m4 m5 = Dynamics (fun p ->
        let a1 = invokeDynamics p m1
        let a2 = invokeDynamics p m2
        let a3 = invokeDynamics p m3
        let a4 = invokeDynamics p m4
        let a5 = invokeDynamics p m5
        in f a1 a2 a3 a4 a5)
        
    [<CompiledName ("Zip")>]
    let zip m1 m2 = Dynamics (fun p ->
        let a1 = invokeDynamics p m1
        let a2 = invokeDynamics p m2
        in (a1, a2)) 
        
    [<CompiledName ("Zip3")>]
    let zip3 m1 m2 m3 = Dynamics (fun p ->
        let a1 = invokeDynamics p m1
        let a2 = invokeDynamics p m2
        let a3 = invokeDynamics p m3
        in (a1, a2, a3)) 
    
    [<CompiledName ("OfList")>]
    let ofList (ms: Dynamics<_> list) = Dynamics (fun p ->
        [ for m in ms -> invokeDynamics p m ])
    
    [<CompiledName ("OfArray")>]
    let ofArray (ms: Dynamics<_> []) = Dynamics (fun p ->
        [| for m in ms -> invokeDynamics p m |])
                  
    [<CompiledName ("OfEnumerable")>]
    let ofSeq (ms: #seq<Dynamics<_>>) = Dynamics (fun p ->
        seq { for m in ms -> invokeDynamics p m })
    
    [<CompiledName ("Discrete")>]
    let discrete m = Dynamics (fun p -> 
    
        if p.Phase = 0 then
            invokeDynamics p m
        else 
            let p' = { p with Time = Specs.integTime p.Specs p.Iteration 0;
                              Phase = 0 }
            invokeDynamics p' m)
   
    [<CompiledName ("Interpolate")>]
    let interpolate m = Dynamics (fun p -> 
    
        if p.Phase >= 0 then
            invokeDynamics p m
        else 
            let p' = { p with Time = Specs.integTime p.Specs p.Iteration 0;
                              Phase = 0 }
            invokeDynamics p' m)
   
    type Memo<'a> = 
        { mutable Iteration: Iteration; 
          mutable Phase: Phase; 
          Data: 'a [][] 
        }
    
    [<CompiledName ("Memo")>]
    let memo (m: Dynamics<'a>) =

        let state =
            Simulation (fun r ->

                let iterationCount = r.Specs.IterationCount
                let phaseCount = r.Specs.PhaseCount

                { Iteration = 0;
                  Phase = 0;
                  Data = [| for i = 0 to phaseCount - 1 do
                                yield Array.zeroCreate iterationCount |] })

            |> Simulation.memo
                
        in Dynamics (fun p ->

            let state = invokeSimulation p.Run state
        
            let n  = p.Iteration
            let ph = p.Phase
        
            if (state.Iteration < n) || (state.Iteration = n) && (state.Phase <= ph) then
            
                let phaseCount = p.Specs.PhaseCount
            
                while (state.Iteration < n) || (state.Iteration = n) && (state.Phase <= ph) do
                
                    let n'  = state.Iteration
                    let ph' = state.Phase
                    let t'  = Specs.integTime p.Specs n' ph'
                    let p'  = { p with Time = t'; Iteration = n'; Phase = ph' }
                    
                    state.Data.[ph'].[n'] <- invokeDynamics p' m
                    
                    if n' <> state.Iteration then failwith "memo: recurrent loop"
                    if ph' <> state.Phase then failwith "memo recurrent loop"
                    
                    state.Phase <- state.Phase + 1
                    
                    if (state.Phase >= phaseCount) then
                        state.Phase <- 0
                        state.Iteration <- state.Iteration + 1
                
            state.Data.[ph].[n])
                
        |> interpolate
    
    type Memo0<'a> = 
        { mutable Iteration: Iteration; 
          Data: 'a []
        }
    
    [<CompiledName ("Memo0")>]
    let memo0 (m: Dynamics<'a>) =

        let state =
            Simulation (fun r ->

                let iterationCount = r.Specs.IterationCount
                
                { Iteration = 0;
                  Data = Array.zeroCreate iterationCount })

            |> Simulation.memo
                
        in Dynamics (fun p ->

            let state = invokeSimulation p.Run state
        
            let n  = p.Iteration
            
            if state.Iteration <= n then
            
                while state.Iteration <= n do
                
                    let n'  = state.Iteration
                    let t'  = Specs.integTime p.Specs n' 0
                    let p'  = { p with Time = t'; Iteration = n'; Phase = 0 }
                    
                    state.Data.[n'] <- invokeDynamics p' m
                    
                    if n' <> state.Iteration then failwith "memo0: recurrent loop"
                    
                    state.Iteration <- state.Iteration + 1
                
            state.Data.[n])
                
        |> discrete
        
    [<CompiledName ("MemoRandomUniform")>]
    let memoRandomUniform minimum maximum =
        Dynamics (fun p ->
            let g = p.Run.Generator
            let minimum' = invokeDynamics p minimum
            let maximum' = invokeDynamics p maximum
            g.NextUniform (minimum', maximum'))
        |> memo0
        
    [<CompiledName ("MemoRandomUniformInt")>]
    let memoRandomUniformInt minimum maximum =
        Dynamics (fun p ->
            let g = p.Run.Generator
            let minimum' = invokeDynamics p minimum
            let maximum' = invokeDynamics p maximum
            g.NextUniformInt (minimum', maximum'))
        |> memo0

    [<CompiledName ("MemoRandomTriangular")>]
    let memoRandomTriangular minimum median maximum =
        Dynamics (fun p ->
            let g = p.Run.Generator
            let minimum' = invokeDynamics p minimum
            let median'  = invokeDynamics p median
            let maximum' = invokeDynamics p maximum
            g.NextTriangular (minimum', median', maximum'))
        |> memo0
    
    [<CompiledName ("MemoRandomNormal")>]
    let memoRandomNormal mean deviation =    
        Dynamics (fun p ->
            let g = p.Run.Generator
            let mean' = invokeDynamics p mean
            let deviation' = invokeDynamics p deviation
            g.NextNormal (mean', deviation'))
        |> memo0
    
    [<CompiledName ("MemoRandomExponential")>]
    let memoRandomExponential mean =
        Dynamics (fun p ->
            let g = p.Run.Generator
            let mean' = invokeDynamics p mean
            g.NextExponential (mean'))
        |> memo0
    
    [<CompiledName ("MemoRandomErlang")>]
    let memoRandomErlang beta m =
        Dynamics (fun p ->
            let g = p.Run.Generator
            let beta' = invokeDynamics p beta
            let m' = invokeDynamics p m
            g.NextErlang (beta', m'))
        |> memo0
    
    [<CompiledName ("MemoRandomPoisson")>]
    let memoRandomPoisson mean =
        Dynamics (fun p ->
            let g = p.Run.Generator
            let mean' = invokeDynamics p mean
            g.NextPoisson (mean'))
        |> memo0
    
    [<CompiledName ("MemoRandomBinomial")>]
    let memoRandomBinomial prob trials = 
        Dynamics (fun p ->
            let g = p.Run.Generator
            let prob' = invokeDynamics p prob
            let trials' = invokeDynamics p trials
            g.NextBinomial (prob', trials'))
        |> memo0

    [<CompiledName ("Time")>]
    let time = Dynamics (fun p -> p.Time)

    [<CompiledName ("IntegIteration")>]
    let integIteration = Dynamics (fun p -> p.Iteration)

    [<CompiledName ("IntegPhase")>]
    let integPhase = Dynamics (fun p -> p.Phase)

    [<CompiledName ("Max")>]
    let max x1 x2 =
        Dynamics (fun p ->
            let x1' = invokeDynamics p x1
            let x2' = invokeDynamics p x2
            in max x1' x2')

    [<CompiledName ("Min")>]
    let min x1 x2 =
        Dynamics (fun p ->
            let x1' = invokeDynamics p x1
            let x2' = invokeDynamics p x2
            in min x1' x2')

    [<CompiledName ("IfThenElse")>]
    let ifThenElse cond thenPart elsePart =
        Dynamics (fun p ->
            if invokeDynamics p cond then
                invokeDynamics p thenPart
            else
                invokeDynamics p elsePart)

    [<CompiledName ("Trace")>]
    let trace message (m: Dynamics<'a>) =
        Dynamics (fun p ->
            printfn "t = %f: %s" p.Time message
            invokeDynamics p m)
         
type Dynamics<'a> with

    static member (~+) (a: Dynamics<float>) = a

    static member (~-) (a: Dynamics<float>) = 
        Dynamics (fun p -> - invokeDynamics p a)

    static member (+) (a: Dynamics<float>, b: Dynamics<float>) = 
        Dynamics (fun p -> invokeDynamics p a + invokeDynamics p b)
        
    static member (+) (a: Dynamics<float>, b: Parameter<float>) = 
        Dynamics (fun p -> invokeDynamics p a + invokeParameter p.Run b)
        
    static member (+) (a: Dynamics<float>, b: float) =
        Dynamics (fun p -> invokeDynamics p a + b)

    static member (+) (a: Parameter<float>, b: Dynamics<float>) = 
        Dynamics (fun p -> invokeParameter p.Run a + invokeDynamics p b)
        
    static member (+) (a: float, b: Dynamics<float>) =
        Dynamics (fun p -> a + invokeDynamics p b)
    
    static member (-) (a: Dynamics<float>, b: Dynamics<float>) =
        Dynamics (fun p -> invokeDynamics p a - invokeDynamics p b)

    static member (-) (a: Dynamics<float>, b: Parameter<float>) =
        Dynamics (fun p -> invokeDynamics p a - invokeParameter p.Run b)
        
    static member (-) (a: Dynamics<float>, b: float) =
        Dynamics (fun p -> invokeDynamics p a - b)

    static member (-) (a: Parameter<float>, b: Dynamics<float>) =
        Dynamics (fun p -> invokeParameter p.Run a - invokeDynamics p b)
        
    static member (-) (a: float, b: Dynamics<float>) =
        Dynamics (fun p -> a - invokeDynamics p b)
    
    static member (*) (a: Dynamics<float>, b: Dynamics<float>) =
        Dynamics (fun p -> invokeDynamics p a * invokeDynamics p b)
        
    static member (*) (a: Dynamics<float>, b: Parameter<float>) =
        Dynamics (fun p -> invokeDynamics p a * invokeParameter p.Run b)
        
    static member (*) (a: Dynamics<float>, b: float) =
        Dynamics (fun p -> invokeDynamics p a * b)

    static member (*) (a: Parameter<float>, b: Dynamics<float>) =
        Dynamics (fun p -> invokeParameter p.Run a * invokeDynamics p b)
        
    static member (*) (a: float, b: Dynamics<float>) =
        Dynamics (fun p -> a * invokeDynamics p b)
    
    static member (/) (a: Dynamics<float>, b: Dynamics<float>) =
        Dynamics (fun p -> (invokeDynamics p a) / (invokeDynamics p b))
        
    static member (/) (a: Dynamics<float>, b: Parameter<float>) =
        Dynamics (fun p -> (invokeDynamics p a) / (invokeParameter p.Run b))
        
    static member (/) (a: Dynamics<float>, b: float) =
        Dynamics (fun p -> (invokeDynamics p a) / b)

    static member (/) (a: Parameter<float>, b: Dynamics<float>) =
        Dynamics (fun p -> (invokeParameter p.Run a) / (invokeDynamics p b))
        
    static member (/) (a: float, b: Dynamics<float>) =
        Dynamics (fun p -> a / (invokeDynamics p b))
        
    static member Abs (a: Dynamics<float>) = 
        Dynamics (fun p -> abs (invokeDynamics p a))
    
    static member Acos (a: Dynamics<float>) = 
        Dynamics (fun p -> acos (invokeDynamics p a))
    
    static member Asin (a: Dynamics<float>) = 
        Dynamics (fun p -> asin (invokeDynamics p a))
    
    static member Atan (a: Dynamics<float>) = 
        Dynamics (fun p -> atan (invokeDynamics p a))
    
    static member Atan2 (a: Dynamics<float>, b: Dynamics<float>) = 
        Dynamics (fun p -> atan2 (invokeDynamics p a) (invokeDynamics p b))

    static member Ceiling (a: Dynamics<float>) = 
        Dynamics (fun p -> ceil (invokeDynamics p a))
    
    static member Exp (a: Dynamics<float>) = 
        Dynamics (fun p -> exp (invokeDynamics p a))
    
    static member Floor (a: Dynamics<float>) = 
        Dynamics (fun p -> floor (invokeDynamics p a))
    
    static member Truncate (a: Dynamics<float>) = 
        Dynamics (fun p -> truncate (invokeDynamics p a))
    
    static member Round (a: Dynamics<float>) = 
        Dynamics (fun p -> round (invokeDynamics p a))
    
    static member Log (a: Dynamics<float>) = 
        Dynamics (fun p -> log (invokeDynamics p a))
    
    static member Log10 (a: Dynamics<float>) = 
        Dynamics (fun p -> log10 (invokeDynamics p a))
    
    static member Sqrt (a: Dynamics<float>) = 
        Dynamics (fun p -> sqrt (invokeDynamics p a))
    
    static member Cos (a: Dynamics<float>) = 
        Dynamics (fun p -> cos (invokeDynamics p a))
    
    static member Cosh (a: Dynamics<float>) = 
        Dynamics (fun p -> cosh (invokeDynamics p a))
    
    static member Sin (a: Dynamics<float>) = 
        Dynamics (fun p -> sin (invokeDynamics p a))
    
    static member Sinh (a: Dynamics<float>) = 
        Dynamics (fun p -> sinh (invokeDynamics p a))
    
    static member Tan (a: Dynamics<float>) = 
        Dynamics (fun p -> tan (invokeDynamics p a))
    
    static member Tanh (a: Dynamics<float>) = 
        Dynamics (fun p -> tanh (invokeDynamics p a))
    
    static member Pow (a: Dynamics<float>, b: Dynamics<float>) = 
        Dynamics (fun p -> (invokeDynamics p a) ** (invokeDynamics p b))
    
    static member Pow (a: Dynamics<float>, b: float) = 
        Dynamics (fun p -> (invokeDynamics p a) ** b)
