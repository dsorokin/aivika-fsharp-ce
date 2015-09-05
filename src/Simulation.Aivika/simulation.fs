
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
type Simulation<'a> (f: Run -> 'a) =

    member internal x.Fun = f

    static member From (m: Parameter<'a>) =
        Simulation (fun r -> invokeParameter r m)

    static member From (m: Simulation<'a>) = m
    
[<AutoOpen>]
module internal SimulationInvoke =

    let inline invokeSimulation r (m: Simulation<'a>) = m.Fun r
    
module SimulationBuilderImpl =

    let inline returnS a =
        Simulation (fun r -> a)

    let inline bindS m k =
        Simulation (fun r ->
            let a = invokeSimulation r m
            let m' = k a
            invokeSimulation r m')

    //  letS a k = bindS (returnS a) k
    let inline letS a k =
        Simulation (fun r ->
            let m' = k a
            invokeSimulation r m')

    //  delayS k = bindS (returnS ()) k
    let inline delayS k =
        Simulation (fun r ->
            let m' = k ()
            invokeSimulation r m')
            
    //  combineS m1 m2 = bindS m1 (fun () -> m2)
    let inline combineS m1 m2 =
        Simulation (fun r ->
            invokeSimulation r m1
            invokeSimulation r m2)
            
    let zeroS = Simulation (fun r -> ())
    
    let inline forS es k  =
        Simulation (fun r ->
            for e in es do
                invokeSimulation r (k e))
                
    let inline whileS p m =
        Simulation (fun r ->
            while p () do
                invokeSimulation r m)
                
    let inline usingS (a: 'a when 'a :> IDisposable) k =
        Simulation (fun r ->
            try
                invokeSimulation r (k a)
            finally
                a.Dispose ())
                
    let inline tryFinallyS m f =
        Simulation (fun r ->
            try
                invokeSimulation r m
            finally
                f ())
                
    let inline tryWithS m k =
        Simulation (fun r ->
            try
                invokeSimulation r m
            with
            | e -> invokeSimulation r (k e))
            
open SimulationBuilderImpl

[<Sealed>]
type SimulationBuilder() =

    member d.Return (a)  = returnS a
    member d.ReturnFrom (m: Simulation<_>) = m
    member d.Bind (m, k) = bindS m k
    member d.Delay (k)   = delayS k
    member d.Zero () = zeroS
    member d.Combine (m1, m2) = combineS m1 m2
    member d.For (es, k) = forS es k
    member d.While (p, m) = whileS p m
    member d.Using (a, k) = usingS a k
    member d.TryFinally (m, f) = tryFinallyS m f
    member d.TryWith (m, k) = tryWithS m k

[<AutoOpen>]
module SimulationWorkflow =
    let simulation = new SimulationBuilder()

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Simulation =

    [<CompiledName ("Run")>]
    let run specs m =

        let r = { Specs = specs;
                  Index = 0;
                  Count = 1;
                  EventQueue = EventQueue (specs.StartTime);
                  UniqueId = UniqueIdGenerator.Next ();
                  Generator = Generator.Create (specs.GeneratorType) }
        
        invokeSimulation r m       

    [<CompiledName ("RunBySeriesIndex")>]
    let runBySeriesIndex count index specs m =

        let r = { Specs = specs;
                  Index = index;
                  Count = count;
                  EventQueue = EventQueue (specs.StartTime);
                  UniqueId = UniqueIdGenerator.Next ();
                  Generator = Generator.Create (specs.GeneratorType) }
        
        invokeSimulation r m       

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
        
                return (invokeSimulation r m) }
    }       

    [<CompiledName ("Lift")>]
    let inline lift (x: Simulation<'a>) = (^m: (static member From: Simulation<'a> -> ^m) (x))

    [<CompiledName ("Concat")>]
    let concat mm = Simulation (fun r ->
        let m = invokeSimulation r mm
        invokeSimulation r m)
    
    [<CompiledName ("Map")>]
    let map f m = Simulation (fun r ->
        let a = invokeSimulation r m
        in f a)
    
    [<CompiledName ("Ap")>]
    let ap mf m = Simulation (fun r ->
        let f = invokeSimulation r mf
        let a = invokeSimulation r m
        in f a)
    
    [<CompiledName ("Lift2")>]
    let lift2 f m1 m2 = Simulation (fun r ->
        let a1 = invokeSimulation r m1
        let a2 = invokeSimulation r m2
        in f a1 a2)
    
    [<CompiledName ("Lift3")>]
    let lift3 f m1 m2 m3 = Simulation (fun r ->
        let a1 = invokeSimulation r m1
        let a2 = invokeSimulation r m2
        let a3 = invokeSimulation r m3
        in f a1 a2 a3)
    
    [<CompiledName ("Lift4")>]
    let lift4 f m1 m2 m3 m4 = Simulation (fun r ->
        let a1 = invokeSimulation r m1
        let a2 = invokeSimulation r m2
        let a3 = invokeSimulation r m3
        let a4 = invokeSimulation r m4
        in f a1 a2 a3 a4)
    
    [<CompiledName ("Lift5")>]
    let lift5 f m1 m2 m3 m4 m5 = Simulation (fun r ->
        let a1 = invokeSimulation r m1
        let a2 = invokeSimulation r m2
        let a3 = invokeSimulation r m3
        let a4 = invokeSimulation r m4
        let a5 = invokeSimulation r m5
        in f a1 a2 a3 a4 a5)
        
    [<CompiledName ("Zip")>]
    let zip m1 m2 = Simulation (fun r ->
        let a1 = invokeSimulation r m1
        let a2 = invokeSimulation r m2
        in (a1, a2)) 
        
    [<CompiledName ("Zip3")>]
    let zip3 m1 m2 m3 = Simulation (fun r ->
        let a1 = invokeSimulation r m1
        let a2 = invokeSimulation r m2
        let a3 = invokeSimulation r m3
        in (a1, a2, a3)) 
    
    [<CompiledName ("OfList")>]
    let ofList (ms: Simulation<_> list) = Simulation (fun r ->
        [ for m in ms -> invokeSimulation r m ])
    
    [<CompiledName ("OfArray")>]
    let ofArray (ms: Simulation<_> []) = Simulation (fun r ->
        [| for m in ms -> invokeSimulation r m |])
                  
    [<CompiledName ("OfEnumerable")>]
    let ofSeq (ms: #seq<Simulation<_>>) = Simulation (fun r ->
        seq { for m in ms -> invokeSimulation r m })
   
    [<CompiledName ("Memo")>]
    let memo (m: Simulation<'a>) =

        let dict = ref None
        let dictlock = new obj ()

        let message = 
            "Detected a logical error when the Simulation.memo computation " +
            "was not created safely within the Simulation computation."
        
        in Simulation (fun r ->
        
            match !dict with
            | Some (uniqueId, a) ->

                if uniqueId = r.UniqueId 
                    then a else raise <| InvalidOperationException message

            | None ->

                lock dictlock (fun () ->

                    match !dict with
                    | Some (uniqueId, a) ->

                        if uniqueId = r.UniqueId 
                            then a else raise <| InvalidOperationException message

                    | None ->

                        let a = invokeSimulation r m
                        dict := Some (r.UniqueId, a)
                        a))

    [<CompiledName ("IfThenElse")>]
    let ifThenElse cond thenPart elsePart =
        Simulation (fun r ->
            if invokeSimulation r cond then
                invokeSimulation r thenPart
            else
                invokeSimulation r elsePart)

type SimulationException (message) =
    inherit Exception (message)

type SimulationAbort (message) =
    inherit SimulationException (message)
