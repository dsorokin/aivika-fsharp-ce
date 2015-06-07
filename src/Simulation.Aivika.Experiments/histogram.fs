
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

namespace Simulation.Aivika.Experiments
        
open System

open Simulation.Aivika

type BinningStrategy = array<float> -> int

type Histogram = array<float * array<int>>
    
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module BinningStrategy =

    [<CompiledName ("Kurtosis")>]
    let kurtosis (xs: float array) =
        let n  = float xs.Length
        let ex = xs |> SamplingStats.fromFloats |> SamplingStats.mean
        let e1 = xs |> Array.sumBy (fun x -> let dx = (x - ex) in dx * dx * dx * dx)
        let e2 = xs |> Array.sumBy (fun x -> let dx = (x - ex) in dx * dx)
        let s1 = e1 / n
        let s2 = e2 / n
        s1 / (s2 * s2) - 3.0

    [<CompiledName ("Sturges")>]
    let sturges (xs: float array) =
        let n = float xs.Length
        int (ceil (Math.Log (n, 2.0) + 1.0))

    [<CompiledName ("Doane")>]
    let doane (xs: float array) =
        let n = float xs.Length
        let a = kurtosis xs
        int (ceil (1.0 + log n + log (1.0 + a * ((n / 6.0) ** 0.5))))

    [<CompiledName ("Sqrt")>]
    let sqrt (xs: float array) =
        let n = float xs.Length
        int (round (sqrt n))

    [<CompiledName ("Scott")>]
    let scott (xs: float array) =
        let n  = float xs.Length
        let tx = xs |> SamplingStats.fromFloats
        let sx = tx |> SamplingStats.deviation
        let minx = tx |> SamplingStats.minimum
        let maxx = tx |> SamplingStats.maximum 
        let h  = 3.5 * sx / (n ** (1.0 / 3.0))
        int (ceil ((maxx - minx) / h))

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Histogram =

    /// Combines bins from different histograms (a generic version).
    let private combineBins' (xs: array<array<float * int>>) : Histogram =
        let count = xs.Length
        let indexing i = Seq.map (fun (t, n) -> (i, t, n))
        let numbering  = Seq.mapi indexing
        let ordering   = Seq.sortBy (fun (_, t, _) -> t)
        let groupping  = Seq.groupBy (fun (_, t, _) -> t)
        let sorting    = Seq.sortBy (fun (i, _, _) -> i)
        let rec merging' zs t k acc =
            match zs with
            | [] when k < count -> merging' [] t (k + 1) (0 :: acc)
            | [] -> (t, List.rev acc |> List.toArray)
            | ((i, _, n) :: xs) when i = k -> merging' xs t (k + 1) (n :: acc)
            | ((i, _, n) :: xs) -> merging' zs t (k + 1) (0 :: acc)
        let merging zs =
            match zs with
            | [] -> (0.0, [| |])
            | ((_, t, _) :: _) -> merging' zs t 0 []
        xs |> numbering
           |> Seq.concat
           |> ordering
           |> groupping
           |> Seq.map (snd >> sorting >> Seq.toList >> merging)
           |> Seq.toArray

    /// Combines bins from different histograms (an optimized version).
    let private combineBins (xs: array<array<float * int>>) : Histogram = 
        match xs with
        | [| xs |] -> xs |> Array.map (fun (t, n) -> (t, [| n |]))
        | xss  -> combineBins' xss

    /// Returns bins the for the histogram.
    let private bins (size: float) (xs: array<float>) : array<float * int> =
        let xs = xs |> Array.sort |> Seq.groupBy id
        [| for (k, vs) in xs do yield (k, Seq.length vs) |]

    /// Rounds numbers so that they would be divisible by size.
    let private roundNums (size: float) (xs: array<float>) : array<float> =
        match xs with
        | [| x |] -> xs
        | _ -> xs |> Array.map (fun x -> size * floor (x / size) + size / 2.0)

    [<CompiledName ("CreateByBinSize")>]
    let createByBinSize (size: float) (xs: array<array<float>>) : Histogram =
        xs |> Array.map (roundNums size >> bins size) |> combineBins

    [<CompiledName ("CreateByBinCount")>]
    let createByBinCount (n: int) (xs: array<array<float>>) : Histogram =
        let exp10 y = floor (log10 y)
        let digit y = floor (y / (10.0 ** (exp10 y)))
        let maxa (xs: float array) = if xs.Length = 0 then Double.NegativeInfinity else Array.max xs
        let mina (xs: float array) = if xs.Length = 0 then Double.PositiveInfinity else Array.min xs
        let maxx  = xs |> Array.map maxa |> maxa
        let minx  = xs |> Array.map mina |> mina
        let diff0 = (maxx - minx) / float (max 1 n)
        let diff  = if diff0 > 0.0 then diff0 else 1.0
        let size  = digit diff * (10.0 ** (exp10 diff))
        createByBinSize size xs 

    [<CompiledName ("Create")>]
    let create (strat: BinningStrategy) (xs: array<array<float>>) : Histogram =
        createByBinCount (xs |> Array.concat |> strat) xs
