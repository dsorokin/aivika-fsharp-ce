
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
open System.Security.Cryptography

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module private BuiltInGeneratorCore = 

    let simple seed =
        match seed with
            | None ->
                let r = Random ()
                in fun () -> r.NextDouble ()
            | Some s ->
                let r = Random (s)
                in fun () -> r.NextDouble ()
                            
    let strong () =
        let r = new RNGCryptoServiceProvider()
        let d = Array.create 8 0uy
        in fun () ->
            
                r.GetBytes (d)
                
                let i1 = uint64 (d.[0] &&& 3uy)
                let i1 = (i1 <<< 8) + uint64 d.[1]
                let i1 = (i1 <<< 8) + uint64 d.[2]
                let i1 = (i1 <<< 8) + uint64 d.[3]
                
                let i2 = uint64 (d.[4] &&& 7uy)
                let i2 = (i2 <<< 8) + uint64 d.[5]
                let i2 = (i2 <<< 8) + uint64 d.[6]
                let i2 = (i2 <<< 8) + uint64 d.[7]
                
                in (float ((i1 <<< 27) + i2)) / ((float) (1L <<< 53))

    let normal g =
    
        let next = ref 0.0
        let flag = ref false
        
        in fun () ->
        
            if !flag then
            
                flag := false
                !next
                
            else
            
                let mutable xi1 = 0.0
                let mutable xi2 = 0.0
                let mutable psi = 0.0
                
                while (psi >= 1.0) || (psi = 0.0) do

                    xi1 <- 2.0 * g () - 1.0
                    xi2 <- 2.0 * g () - 1.0
                    psi <- xi1 * xi1 + xi2 * xi2

                psi <- sqrt (- 2.0 * (log psi) / psi)
                
                flag := true
                next := xi2 * psi
                
                xi1 * psi

    let poisson mu g =
    
        let mutable v = 0
        let mutable prob = g ()
        let mutable prod = exp (- mu)
        
        while (prob > prod) do

            v <- v + 1
            prob <- prob - prod
            prod <- prod * mu / (float v)
            
        v

    let binomial prob trials g =
        let mutable sum = 0
        for i = 1 to trials do
            if g () <= prob then sum <- sum + 1
        sum

    let erlang beta m g =
        let mutable prod = 1.0
        for i = 1 to m do
            prod <- prod * g ()
        (- log (prod)) * beta

type GeneratorType =
    | SimpleGenerator
    | SimpleGeneratorWithSeed of int
    | StrongGenerator
    | CustomGenerator of (unit -> Generator)

and [<AbstractClass>] Generator () =

    static member Create (tp) = 
        match tp with
        | SimpleGenerator ->
            BasicGenerator (BuiltInGeneratorCore.simple None)
                :> Generator
        | SimpleGeneratorWithSeed x ->
            BasicGenerator (BuiltInGeneratorCore.simple <| Some x)
                :> Generator
        | StrongGenerator ->
            BasicGenerator (BuiltInGeneratorCore.strong ())
                :> Generator
        | CustomGenerator f ->
            f ()

    abstract NextUniform: unit -> float

    abstract NextUniform: minimum:float * maximum:float -> float

    abstract NextUniformInt: minimum:int * maximum:int -> int

    abstract NextNormal: unit -> float
    
    abstract NextNormal: mean:float * deviation:float -> float

    abstract NextExponential: mean:float -> float

    abstract NextErlang: beta:float * m:int -> float
    
    abstract NextPoisson: mean:float -> int
    
    abstract NextBinomial: prob:float * trials:int -> int  

and BasicGenerator (uniformGenerator: unit -> float) =

    inherit Generator ()
            
    let normalGenerator =
        BuiltInGeneratorCore.normal uniformGenerator

    override x.NextUniform () =
        uniformGenerator ()

    override x.NextUniform (minimum, maximum) =
        minimum + (maximum - minimum) * uniformGenerator ()

    override x.NextUniformInt (minimum, maximum) =
        let minimum' = float minimum
        let maximum' = float maximum
        int (round (minimum' + (maximum' - minimum') * uniformGenerator ()))

    override x.NextNormal () =
        normalGenerator ()
    
    override x.NextNormal (mean, deviation) =
        mean + deviation * normalGenerator ()

    override x.NextExponential (mean) =
        - log (uniformGenerator ()) * mean

    override x.NextErlang (beta, m) =
        BuiltInGeneratorCore.erlang beta m uniformGenerator
    
    override x.NextPoisson (mean) =
        BuiltInGeneratorCore.poisson mean uniformGenerator
    
    override x.NextBinomial (prob, trials) = 
        BuiltInGeneratorCore.binomial prob trials uniformGenerator 
        
