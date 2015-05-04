
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
open System.Collections.Generic

[<Sealed; NoComparison; NoEquality>]
type Parameter<'a> (f: Run -> 'a) =

    member internal x.Fun = f

    static member From (m: Parameter<'a>) = m
    
[<AutoOpen>]
module internal ParameterInvoke =

    let inline invokeParameter r (m: Parameter<'a>) = m.Fun r
    
module ParameterBuilderImpl =

    let inline returnP a =
        Parameter (fun r -> a)

    let inline bindP m k =
        Parameter (fun r ->
            let a = invokeParameter r m
            let m' = k a
            invokeParameter r m')

    //  letP a k = bindP (returnP a) k
    let inline letP a k =
        Parameter (fun r ->
            let m' = k a
            invokeParameter r m')

    //  delayP k = bindP (returnP ()) k
    let inline delayP k =
        Parameter (fun r ->
            let m' = k ()
            invokeParameter r m')
            
    //  combineP m1 m2 = bindP m1 (fun () -> m2)
    let inline combineP m1 m2 =
        Parameter (fun r ->
            invokeParameter r m1
            invokeParameter r m2)
            
    let zeroP = Parameter (fun r -> ())
    
    let inline forP es k  =
        Parameter (fun r ->
            for e in es do
                invokeParameter r (k e))
                
    let inline whileP p m =
        Parameter (fun r ->
            while p () do
                invokeParameter r m)
                
    let inline usingP (a: 'a when 'a :> IDisposable) k =
        Parameter (fun r ->
            try
                invokeParameter r (k a)
            finally
                a.Dispose ())
                
    let inline tryFinallyP m f =
        Parameter (fun r ->
            try
                invokeParameter r m
            finally
                f ())
                
    let inline tryWithP m k =
        Parameter (fun r ->
            try
                invokeParameter r m
            with
            | e -> invokeParameter r (k e))
            
open ParameterBuilderImpl

[<Sealed>]
type ParameterBuilder() =

    member d.Return (a)  = returnP a
    member d.ReturnFrom (m: Parameter<_>) = m
    member d.Bind (m, k) = bindP m k
    member d.Delay (k)   = delayP k
    member d.Zero () = zeroP
    member d.Combine (m1, m2) = combineP m1 m2
    member d.For (es, k) = forP es k
    member d.While (p, m) = whileP p m
    member d.Using (a, k) = usingP a k
    member d.TryFinally (m, f) = tryFinallyP m f
    member d.TryWith (m, k) = tryWithP m k

[<AutoOpen>]
module ParameterWorkflow =
    let parameter = new ParameterBuilder()

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Parameter =

    [<CompiledName ("Run")>]
    let run specs m =

        let r = { Specs = specs;
                  Index = 0;
                  Count = 1;
                  EventQueue = EventQueue (specs.StartTime);
                  UniqueId = UniqueIdGenerator.Next ();
                  Generator = Generator.Create (specs.GeneratorType) }
        
        invokeParameter r m       

    [<CompiledName ("RunSeries")>]
    let runSeries count specs m = seq {

        for i = 0 to count - 1 do

            yield async {

                let r = { Specs = specs;
                          Index = i;
                          Count = count;
                          EventQueue = EventQueue (specs.StartTime);
                          UniqueId = UniqueIdGenerator.Next ();
                          Generator = Generator.Create (specs.GeneratorType) }
        
                return (invokeParameter r m) }
    }       

    [<CompiledName ("Lift")>]
    let inline lift (x: Parameter<'a>) = (^m: (static member From: Parameter<'a> -> ^m) (x))

    [<CompiledName ("Concat")>]
    let concat mm = Parameter (fun r ->
        let m = invokeParameter r mm
        invokeParameter r m)
    
    [<CompiledName ("Map")>]
    let map f m = Parameter (fun r ->
        let a = invokeParameter r m
        in f a)
    
    [<CompiledName ("Ap")>]
    let ap mf m = Parameter (fun r ->
        let f = invokeParameter r mf
        let a = invokeParameter r m
        in f a)
    
    [<CompiledName ("Lift2")>]
    let lift2 f m1 m2 = Parameter (fun r ->
        let a1 = invokeParameter r m1
        let a2 = invokeParameter r m2
        in f a1 a2)
    
    [<CompiledName ("Lift3")>]
    let lift3 f m1 m2 m3 = Parameter (fun r ->
        let a1 = invokeParameter r m1
        let a2 = invokeParameter r m2
        let a3 = invokeParameter r m3
        in f a1 a2 a3)
    
    [<CompiledName ("Lift4")>]
    let lift4 f m1 m2 m3 m4 = Parameter (fun r ->
        let a1 = invokeParameter r m1
        let a2 = invokeParameter r m2
        let a3 = invokeParameter r m3
        let a4 = invokeParameter r m4
        in f a1 a2 a3 a4)
    
    [<CompiledName ("Lift5")>]
    let lift5 f m1 m2 m3 m4 m5 = Parameter (fun r ->
        let a1 = invokeParameter r m1
        let a2 = invokeParameter r m2
        let a3 = invokeParameter r m3
        let a4 = invokeParameter r m4
        let a5 = invokeParameter r m5
        in f a1 a2 a3 a4 a5)
        
    [<CompiledName ("Zip")>]
    let zip m1 m2 = Parameter (fun r ->
        let a1 = invokeParameter r m1
        let a2 = invokeParameter r m2
        in (a1, a2)) 
        
    [<CompiledName ("Zip3")>]
    let zip3 m1 m2 m3 = Parameter (fun r ->
        let a1 = invokeParameter r m1
        let a2 = invokeParameter r m2
        let a3 = invokeParameter r m3
        in (a1, a2, a3)) 
    
    [<CompiledName ("OfList")>]
    let ofList (ms: Parameter<_> list) = Parameter (fun r ->
        [ for m in ms -> invokeParameter r m ])
    
    [<CompiledName ("OfArray")>]
    let ofArray (ms: Parameter<_> []) = Parameter (fun r ->
        [| for m in ms -> invokeParameter r m |])
                  
    [<CompiledName ("OfEnumerable")>]
    let ofSeq (ms: #seq<Parameter<_>>) = Parameter (fun r ->
        seq { for m in ms -> invokeParameter r m })
   
    [<CompiledName ("Memo")>]
    let memo (m: Parameter<'a>) =

        let dict = ref Map.empty
        let dictlock = new obj ()

        let message = 
            "Detected a logical error when the external parameter " +
            "was misused repeatedly. Such a parameter should be " +
            "created anew for each new simulation run " +
            "especially if it performs some side effect."
        
        in Parameter (fun r ->
        
            let k = r.Index

            if Map.containsKey k !dict then

                let (uniqueId, a) = Map.find k !dict
                if uniqueId = r.UniqueId 
                    then a else raise <| InvalidOperationException message

            else

                lock dictlock (fun () ->

                    if Map.containsKey k !dict then

                        let (uniqueId, a) = Map.find k !dict
                        if uniqueId = r.UniqueId 
                            then a else raise <| InvalidOperationException message

                    else

                        let a = invokeParameter r m
                        dict := Map.add k (r.UniqueId, a) !dict
                        a))

    [<CompiledName ("Specs")>]
    let specs = Parameter (fun r -> r.Specs)

    [<CompiledName ("StartTime")>]
    let starttime = Parameter (fun r -> r.Specs.StartTime)

    [<CompiledName ("StopTime")>]
    let stoptime = Parameter (fun r -> r.Specs.StopTime)

    [<CompiledName ("DT")>]
    let dt = Parameter (fun r -> r.Specs.DT)

    [<CompiledName ("RunIndex")>]
    let runIndex = Parameter (fun r -> r.Index)

    [<CompiledName ("RunCount")>]
    let runCount = Parameter (fun r -> r.Count)
    
    [<CompiledName ("FromArray")>]
    let fromArray (arr: 'a []) =
        Parameter (fun r -> arr.[r.Index % arr.Length])
    
    [<CompiledName ("Generator")>]
    let generator = Parameter (fun r -> r.Generator)
    
    [<CompiledName ("RandomUniform")>]
    let randomUniform minimum maximum =
        Parameter (fun r -> r.Generator.NextUniform (minimum, maximum))
    
    [<CompiledName ("RandomUniformInt")>]
    let randomUniformInt minimum maximum =
        Parameter (fun r -> r.Generator.NextUniformInt (minimum, maximum))
    
    [<CompiledName ("RandomNormal")>]
    let randomNormal mean deviation =
        Parameter (fun r -> r.Generator.NextNormal (mean, deviation))
    
    [<CompiledName ("RandomExponential")>]
    let randomExponential mean =
        Parameter (fun r -> r.Generator.NextExponential (mean))
    
    [<CompiledName ("RandomErlang")>]
    let randomErlang beta m =
        Parameter (fun r -> r.Generator.NextErlang (beta, m))
    
    [<CompiledName ("RandomPoisson")>]
    let randomPoisson mean =
        Parameter (fun r -> r.Generator.NextPoisson (mean))
    
    [<CompiledName ("RandomBinomial")>]
    let randomBinomial prob trials =
        Parameter (fun r -> r.Generator.NextBinomial (prob, trials))

    [<CompiledName ("RandomTrue")>]
    let randomTrue prob =
        Parameter (fun r -> r.Generator.NextUniform () <= prob)

    [<CompiledName ("RandomFalse")>]
    let randomFalse prob =
        Parameter (fun r -> r.Generator.NextUniform () > prob)
   
type Parameter<'a> with

    static member (~+) (a: Parameter<float>) = a

    static member (~-) (a: Parameter<float>) = 
        Parameter (fun r -> - invokeParameter r a)

    static member (+) (a: Parameter<float>, b: Parameter<float>) = 
        Parameter (fun r -> invokeParameter r a + invokeParameter r b)
        
    static member (+) (a: Parameter<float>, b: float) =
        Parameter (fun r -> invokeParameter r a + b)
        
    static member (+) (a: float, b: Parameter<float>) =
        Parameter (fun r -> a + invokeParameter r b)
    
    static member (-) (a: Parameter<float>, b: Parameter<float>) =
        Parameter (fun r -> invokeParameter r a - invokeParameter r b)
        
    static member (-) (a: Parameter<float>, b: float) =
        Parameter (fun r -> invokeParameter r a - b)
        
    static member (-) (a: float, b: Parameter<float>) =
        Parameter (fun r -> a - invokeParameter r b)
    
    static member (*) (a: Parameter<float>, b: Parameter<float>) =
        Parameter (fun r -> invokeParameter r a * invokeParameter r b)
        
    static member (*) (a: Parameter<float>, b: float) =
        Parameter (fun r -> invokeParameter r a * b)
        
    static member (*) (a: float, b: Parameter<float>) =
        Parameter (fun r -> a * invokeParameter r b)
    
    static member (/) (a: Parameter<float>, b: Parameter<float>) =
        Parameter (fun r -> (invokeParameter r a) / (invokeParameter r b))
        
    static member (/) (a: Parameter<float>, b: float) =
        Parameter (fun r -> (invokeParameter r a) / b)
        
    static member (/) (a: float, b: Parameter<float>) =
        Parameter (fun r -> a / (invokeParameter r b))
        
    static member Abs (a: Parameter<float>) = 
        Parameter (fun r -> abs (invokeParameter r a))
    
    static member Acos (a: Parameter<float>) = 
        Parameter (fun r -> acos (invokeParameter r a))
    
    static member Asin (a: Parameter<float>) = 
        Parameter (fun r -> asin (invokeParameter r a))
    
    static member Atan (a: Parameter<float>) = 
        Parameter (fun r -> atan (invokeParameter r a))
    
    static member Atan2 (a: Parameter<float>, b: Parameter<float>) = 
        Parameter (fun r -> atan2 (invokeParameter r a) (invokeParameter r b))

    static member Ceiling (a: Parameter<float>) = 
        Parameter (fun r -> ceil (invokeParameter r a))
    
    static member Exp (a: Parameter<float>) = 
        Parameter (fun r -> exp (invokeParameter r a))
    
    static member Floor (a: Parameter<float>) = 
        Parameter (fun r -> floor (invokeParameter r a))
    
    static member Truncate (a: Parameter<float>) = 
        Parameter (fun r -> truncate (invokeParameter r a))
    
    static member Round (a: Parameter<float>) = 
        Parameter (fun r -> round (invokeParameter r a))
    
    static member Log (a: Parameter<float>) = 
        Parameter (fun r -> log (invokeParameter r a))
    
    static member Log10 (a: Parameter<float>) = 
        Parameter (fun r -> log10 (invokeParameter r a))
    
    static member Sqrt (a: Parameter<float>) = 
        Parameter (fun r -> sqrt (invokeParameter r a))
    
    static member Cos (a: Parameter<float>) = 
        Parameter (fun r -> cos (invokeParameter r a))
    
    static member Cosh (a: Parameter<float>) = 
        Parameter (fun r -> cosh (invokeParameter r a))
    
    static member Sin (a: Parameter<float>) = 
        Parameter (fun r -> sin (invokeParameter r a))
    
    static member Sinh (a: Parameter<float>) = 
        Parameter (fun r -> sinh (invokeParameter r a))
    
    static member Tan (a: Parameter<float>) = 
        Parameter (fun r -> tan (invokeParameter r a))
    
    static member Tanh (a: Parameter<float>) = 
        Parameter (fun r -> tanh (invokeParameter r a))
    
    static member Pow (a: Parameter<float>, b: Parameter<float>) = 
        Parameter (fun r -> (invokeParameter r a) ** (invokeParameter r b))
    
    static member Pow (a: Parameter<float>, b: float) = 
        Parameter (fun r -> (invokeParameter r a) ** b)
