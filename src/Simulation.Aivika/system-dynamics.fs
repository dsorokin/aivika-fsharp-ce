
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

module SD = 

    [<CompiledName ("Num")>]
    let inline num a = dynamics.Return (a)
    
    let inline (.=.) x y = Dynamics.lift2 (=) x y
    
    let inline (.<>.) x y = Dynamics.lift2 (<>) x y
    
    let inline (.<.) x y = Dynamics.lift2 (<) x y
    
    let inline (.>=.) x y = Dynamics.lift2 (>=) x y
    
    let inline (.>.) x y = Dynamics.lift2 (>) x y
    
    let inline (.<=.) x y = Dynamics.lift2 (<=) x y

    [<CompiledName ("IfThenElse")>]
    let ifThenElse cond thenPart elsePart =
        Dynamics (fun p ->
            if invokeDynamics p cond then
                invokeDynamics p thenPart
            else
                invokeDynamics p elsePart)

    type IntegImpl =
    
        static member Euler (y: Lazy<Dynamics<_>>, f: Lazy<Dynamics<_>>, i: Dynamics<_>, p: Point) = 
        
            let s  = p.Specs
            let n  = p.Iteration
            let ph = p.Phase
            
            match ph with
                | 0 ->
                    if n = 0 then
                        let vi = invokeDynamics p i
                        in vi
                    else
                        let ty = Specs.integTime s (n-1) 0
                        let t1 = ty
                        
                        let py = { p with Time = ty; Iteration = n-1; Phase = 0 }
                        let p1 = py
                        
                        let vy = invokeDynamics py y.Value
                        let k1 = invokeDynamics p1 f.Value
                        
                        in vy + s.DT * k1
                | _ -> 
                    failwithf "integ (Euler): incorrect phase = %i" ph 
    
        static member EulerChoice (y: Lazy<Dynamics<_>>, f: Lazy<Dynamics<Choice<_, _>>>, i: Dynamics<_>, p: Point) = 
        
            let s  = p.Specs
            let n  = p.Iteration

            if n = 0 then
                let vi = invokeDynamics p i
                in vi
            else
                let ty = Specs.integTime s (n-1) 0
                let t1 = ty
                
                let py = { p with Time = ty; Iteration = n-1; Phase = 0 }
                let p1 = py

                match invokeDynamics p1 f.Value with

                | Choice1Of2 vy -> vy 
                | Choice2Of2 k1 ->
                
                    let vy = invokeDynamics py y.Value
                    in vy + s.DT * k1
    
        static member RK2 (y: Lazy<Dynamics<_>>, f: Lazy<Dynamics<_>>, i: Dynamics<_>, p: Point) = 
        
            let s  = p.Specs
            let n  = p.Iteration
            let ph = p.Phase
            
            match ph with
                | 0 ->
                    if n = 0 then
                        let vi = invokeDynamics p i
                        in vi
                    else
                        let ty = Specs.integTime s (n-1) 0
                        let t1 = ty
                        let t2 = Specs.integTime s (n-1) 1
                        
                        let py = { p with Time = ty; Iteration = n-1; Phase = 0 }
                        let p1 = py
                        let p2 = { p with Time = t2; Iteration = n-1; Phase = 1 }
                        
                        let vy = invokeDynamics py y.Value
                        let k1 = invokeDynamics p1 f.Value
                        let k2 = invokeDynamics p2 f.Value
                        
                        in vy + s.DT/2.0 * (k1 + k2)
                | 1 ->
                    let ty = Specs.integTime s n 0
                    let t1 = ty
                    
                    let py = { p with Time = ty; Iteration = n; Phase = 0 }
                    let p1 = py
                    
                    let vy = invokeDynamics py y.Value
                    let k1 = invokeDynamics p1 f.Value
                    
                    in vy + s.DT * k1
                | _ -> 
                    failwithf "integ (RK2): incorrect phase = %i" ph 
    
        static member RK4 (y: Lazy<Dynamics<_>>, f: Lazy<Dynamics<_>>, i: Dynamics<_>, p: Point) = 
        
            let s  = p.Specs
            let n  = p.Iteration
            let ph = p.Phase
        
            match ph with
                | 0 -> 
                    if n = 0 then
                        let vi = invokeDynamics p i
                        in vi
                    else
                        let ty = Specs.integTime s (n-1) 0
                        let t1 = ty
                        let t2 = Specs.integTime s (n-1) 1
                        let t3 = Specs.integTime s (n-1) 2
                        let t4 = Specs.integTime s (n-1) 3
                        
                        let py = { p with Time = ty; Iteration = n-1; Phase = 0 }
                        let p1 = py
                        let p2 = { p with Time = t2; Iteration = n-1; Phase = 1 }
                        let p3 = { p with Time = t3; Iteration = n-1; Phase = 2 }
                        let p4 = { p with Time = t4; Iteration = n-1; Phase = 3 }
                        
                        let vy = invokeDynamics py y.Value
                        let k1 = invokeDynamics p1 f.Value
                        let k2 = invokeDynamics p2 f.Value
                        let k3 = invokeDynamics p3 f.Value
                        let k4 = invokeDynamics p4 f.Value
                        
                        in vy + s.DT/6.0 * (k1 + 2.0*k2 + 2.0*k3 + k4)
                | 1 ->   
                    let ty = Specs.integTime s n 0
                    let t1 = ty
                    
                    let py = { p with Time = ty; Iteration = n; Phase = 0 }
                    let p1 = py
                    
                    let vy = invokeDynamics py y.Value
                    let k1 = invokeDynamics p1 f.Value
                    
                    in vy + s.DT/2.0 * k1
                | 2 ->
                    let ty = Specs.integTime s n 0
                    let t2 = Specs.integTime s n 1
                    
                    let py = { p with Time = ty; Iteration = n; Phase = 0 }
                    let p2 = { p with Time = t2; Iteration = n; Phase = 1 }
                    
                    let vy = invokeDynamics py y.Value
                    let k2 = invokeDynamics p2 f.Value
                    
                    in vy + s.DT/2.0 * k2
                | 3 ->
                    let ty = Specs.integTime s n 0
                    let t3 = Specs.integTime s n 2
                    
                    let py = { p with Time = ty; Iteration = n; Phase = 0 }
                    let p3 = { p with Time = t3; Iteration = n; Phase = 2 }
                    
                    let vy = invokeDynamics py y.Value
                    let k3 = invokeDynamics p3 f.Value
                    in vy + s.DT * k3
                | _ -> 
                    failwithf "integ (RK4): incorrect phase = %i" ph 
    
    [<CompiledName ("Integ")>]
    let integ (f: Lazy<Dynamics<float>>) (i: Dynamics<float>) =
    
        let rec y = lazy (Dynamics.memo z)
        and z = Dynamics (fun p -> 
            match p.Specs.Method with
                | Euler -> IntegImpl.Euler (y, f, i, p)
                | RungeKutta2 -> IntegImpl.RK2 (y, f, i, p)
                | RungeKutta4 -> IntegImpl.RK4 (y, f, i, p))
        in y.Value

    [<CompiledName ("IntegChoice")>]
    let integChoice (f: Lazy<Dynamics<Choice<float, float>>>) (i: Dynamics<float>) =
    
        let rec y = lazy (Dynamics.memo z)
        and z = Dynamics (fun p -> IntegImpl.EulerChoice (y, f, i, p))
        in y.Value
        
    [<CompiledName ("Smooth")>]
    let smooth (x: Dynamics<float>) (t: Lazy<Dynamics<float>>) =
        let rec y = integ (lazy ((x - y) / t.Value)) x in y
        
    [<CompiledName ("SmoothI")>]
    let smoothI (x: Lazy<Dynamics<float>>) (t: Lazy<Dynamics<float>>) (i: Dynamics<float>) =
        let rec y = integ (lazy ((x.Value - y) / t.Value)) i in y
        
    [<CompiledName ("Smooth3")>]
    let smooth3 (x: Dynamics<float>) (t: Lazy<Dynamics<float>>) =
        
        let rec y = integ (lazy ((s1 - y) / (t.Value / 3.0))) x
        and s1 = integ (lazy ((s0 - s1) / (t.Value / 3.0))) x
        and s0 = integ (lazy ((x - s0) / (t.Value / 3.0))) x
        in y
        
    [<CompiledName ("Smooth3I")>]
    let smooth3I (x: Lazy<Dynamics<float>>) (t: Lazy<Dynamics<float>>) (i: Dynamics<float>) =
        
        let rec y = integ (lazy ((s1 - y) / (t.Value / 3.0))) i
        and s1 = integ (lazy ((s0 - s1) / (t.Value / 3.0))) i
        and s0 = integ (lazy ((x.Value - s0) / (t.Value / 3.0))) i
        in y
        
    [<CompiledName ("SmoothN")>]
    let smoothN (x: Dynamics<float>) (t: Lazy<Dynamics<float>>) n =
    
        let rec s = [| 
            for k = 0 to n-1 do
                if k = 0 then
                    yield integ (lazy ((x - s.[k]) / (t.Value / (float n)))) x
                else
                    yield integ (lazy ((s.[k-1] - s.[k]) / (t.Value / (float n)))) x |]
        in s.[n-1]
        
    [<CompiledName ("SmoothNI")>]
    let smoothNI (x: Lazy<Dynamics<float>>) (t: Lazy<Dynamics<float>>) n (i: Dynamics<float>) =
    
        let rec s = [| 
            for k = 0 to n-1 do
                if k = 0 then
                    yield integ (lazy ((x.Value - s.[k]) / (t.Value / (float n)))) i
                else
                    yield integ (lazy ((s.[k-1] - s.[k]) / (t.Value / (float n)))) i |]
        in s.[n-1]
        
    [<CompiledName ("Delay1")>]
    let delay1 (x: Dynamics<float>) (t: Dynamics<float>) =
        let rec y = integ (lazy (x - y)) (x*t) / t in y
        
    [<CompiledName ("Delay1I")>]
    let delay1I (x: Lazy<Dynamics<float>>) (t: Dynamics<float>) (i: Dynamics<float>) =
        let rec y = integ (lazy (x.Value - y)) (i*t) / t in y
        
    [<CompiledName ("Delay3")>]
    let delay3 (x: Dynamics<float>) (t: Dynamics<float>) =
        
        let rec y = integ (lazy (s1 - y)) (x*(t/3.0)) / (t/3.0)
        and s1 = integ (lazy (s0 - s1)) (x*(t/3.0)) / (t/3.0)
        and s0 = integ (lazy (x - s0)) (x*(t/3.0)) / (t/3.0)
        in y
        
    [<CompiledName ("Delay3I")>]
    let delay3I (x: Lazy<Dynamics<float>>) (t: Dynamics<float>) (i: Dynamics<float>) =
        
        let rec y = integ (lazy (s1 - y)) (i*(t/3.0)) / (t/3.0)
        and s1 = integ (lazy (s0 - s1)) (i*(t/3.0)) / (t/3.0)
        and s0 = integ (lazy (x.Value - s0)) (i*(t/3.0)) / (t/3.0)
        in y
        
    [<CompiledName ("DelayN")>]
    let delayN (x: Dynamics<float>) (t: Dynamics<float>) n =
    
        let rec s = [| 
            for k = 0 to n-1 do
                if k = 0 then
                    yield integ (lazy (x - s.[k])) (x*(t/(float n))) / (t/(float n))
                else
                    yield integ (lazy (s.[k-1] - s.[k])) (x*(t/(float n))) / (t/(float n)) |]
        in s.[n-1]
        
    [<CompiledName ("DelayNI")>]
    let delayNI (x: Lazy<Dynamics<float>>) (t: Dynamics<float>) n (i: Dynamics<float>) =
    
        let rec s = [| 
            for k = 0 to n-1 do
                if k = 0 then
                    yield integ (lazy (x.Value - s.[k])) (i*(t/(float n))) / (t/(float n))
                else
                    yield integ (lazy (s.[k-1] - s.[k])) (i*(t/(float n))) / (t/(float n)) |]
        in s.[n-1]
        
    [<CompiledName ("Forecast")>]
    let forecast (x: Dynamics<float>) (at: Dynamics<float>) (hz: Dynamics<float>) =
        x * (1.0 + (x / smooth x (lazy at) - 1.0) / at * hz)
        
    [<CompiledName ("Trend")>]
    let trend (x: Dynamics<float>) (at: Dynamics<float>) (i: Dynamics<float>) =
        (x / smoothI (lazy x) (lazy at) (x / (1.0 + i * at)) - 1.0) / at

    [<CompiledName ("DiffSum")>]
    let diffsum (diff: Lazy<Dynamics<float>>) i =
        let rec y =
            Dynamics (fun p ->
                match p.Iteration with
                | 0 ->
                    invokeDynamics p i
                | n ->
                    let s  = p.Specs
                    let ty = Specs.integTime s (n-1) 0
                    let py = { p with Time = ty;
                                      Iteration = n - 1;
                                      Phase = 0 }
                    let a = invokeDynamics py y
                    let b = invokeDynamics py diff.Value
                    a + b)
            |> Dynamics.memo0
        in y

    [<CompiledName ("DiffSumChoice")>]
    let diffsumChoice (diff: Lazy<Dynamics<Choice<float, float>>>) i =
        let rec y =
            Dynamics (fun p ->
                match p.Iteration with
                | 0 ->
                    invokeDynamics p i
                | n ->
                    let s  = p.Specs
                    let ty = Specs.integTime s (n-1) 0
                    let py = { p with Time = ty;
                                      Iteration = n - 1;
                                      Phase = 0 }
                    match invokeDynamics py diff.Value with
                    | Choice1Of2 a -> a 
                    | Choice2Of2 b ->
                        let a = invokeDynamics py y
                        a + b)
            |> Dynamics.memo0
        in y

    [<CompiledName ("Lookup")>]
    let lookup (x: Dynamics<float>) (t: Table)  = 
        Dynamics (fun p -> t.Lookup (invokeDynamics p x))

    [<CompiledName ("LookupStepwise")>]
    let lookupStepwise (x: Dynamics<float>) (t: Table) = 
        Dynamics (fun p -> t.LookupStepwise (invokeDynamics p x))

    [<CompiledName ("Delay")>]
    let delay (x: Dynamics<'a>) (d: Dynamics<float>) = 
    
        Dynamics (fun p ->
        
            let t = p.Time
            let s = p.Specs
            let n = p.Iteration
        
            let t' = t - invokeDynamics p d
            let n' = int (floor ((t' - s.StartTime) / s.DT))
            
            if n' < 0 then
                x |> invokeDynamics { p with Time = s.StartTime; Iteration = 0; Phase = 0 }
            elif n' < n then
                x |> invokeDynamics { p with Time = t'; Iteration = n'; Phase = -1 }
            elif n' > n then
                failwith "delay: cannot return the future data"
            else
                failwith "delay: cannot return the current data")
                
        |> Dynamics.discrete

    [<CompiledName ("DelayI")>]
    let delayI (x: Lazy<Dynamics<'a>>) (d: Dynamics<float>) (i: Dynamics<'a>) = 
    
        Dynamics (fun p ->
        
            let t = p.Time
            let s = p.Specs
            let n = p.Iteration
        
            let t' = t - invokeDynamics p d
            let n' = int (floor ((t' - s.StartTime) / s.DT))
            
            if n' < 0 then
                i |> invokeDynamics { p with Time = s.StartTime; Iteration = 0; Phase = 0 }
            elif n' < n then
                x.Value |> invokeDynamics { p with Time = t'; Iteration = n'; Phase = -1 }
            elif n' > n then
                failwith "delayI: cannot return the future data"
            else
                failwith "delayI: cannot return the current data")
                
        |> Dynamics.memo0

    [<CompiledName ("Step")>]
    let step (h: Dynamics<float>) (st: Dynamics<float>) = 
        Dynamics (fun p ->
            let s = p.Specs
            let t = p.Time
            let st' = invokeDynamics p st
            let t'  = t + s.DT / 2.0
            if st' < t' then
                invokeDynamics p h
            else
                0.0) 
        |> Dynamics.discrete

    [<CompiledName ("Pulse")>]
    let pulse (st: Dynamics<float>) (w: Dynamics<float>) = 
        Dynamics (fun p ->
            let s = p.Specs
            let t = p.Time
            let st' = invokeDynamics p st
            let t'  = t + s.DT / 2.0
            if st' < t' then
                let w' = invokeDynamics p w
                if t' < st' + w' then 1.0 else 0.0
            else
                0.0) 
        |> Dynamics.discrete
        
    [<CompiledName ("PulseP")>]
    let pulseP (st: Dynamics<float>) (w: Dynamics<float>) (period: Dynamics<float>) =
        Dynamics (fun p ->
            let s = p.Specs
            let t = p.Time
            let p'  = invokeDynamics p period
            let st' = invokeDynamics p st
            let y'  = if p' > 0.0 && t > st' then floor ((t - st') / p') * p' else 0.0
            let st' = st' + y'
            let t'  = t + s.DT / 2.0
            if st' < t' then
                let w' = invokeDynamics p w
                if t' < st' + w' then 1.0 else 0.0
            else
                0.0) 
        |> Dynamics.discrete
        
    [<CompiledName ("Ramp")>]
    let ramp (slope: Dynamics<float>) (st: Dynamics<float>) (e: Dynamics<float>) =
        Dynamics (fun p ->
            let s = p.Specs
            let t = p.Time
            let st' = invokeDynamics p st
            if st' < t then
                let slope' = invokeDynamics p slope
                let e' = invokeDynamics p e
                if t < e' then
                    slope' * (t - st')
                else
                    slope' * (e' - st')
            else
                0.0)
        |> Dynamics.discrete
    
    [<CompiledName ("NPV")>]
    let npv stream rate init factor =
        let rec dt' = Parameter.dt |> Parameter.lift
        and df = integ (lazy (- df * rate)) (num 1.0)
        and accum = integ (lazy (stream * df)) init
        in (accum + dt' * stream * df) * factor
    
    [<CompiledName ("NPVE")>]
    let npve stream rate init factor =
        let rec dt' = Parameter.dt |> Parameter.lift
        and df = integ 
                    (lazy (- df * rate / (num 1.0 + rate * dt'))) 
                    (num 1.0 / (num 1.0 + rate * dt'))
        and accum = integ (lazy (stream * df)) init
        in (accum + dt' * stream * df) * factor
