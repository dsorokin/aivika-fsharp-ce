
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

type Circuit<'a, 'b> = Wire<'a> -> Wire<'b>

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Circuit =

    [<CompiledName ("Run")>]
    let run xs (p: Circuit<_, _>) = p xs

    [<CompiledName ("Arr")>]
    let arr f = Wire.map f
    
    [<CompiledName ("ArrC")>]
    let arrc f = Wire.mapc f

    [<CompiledName ("Accum")>]
    let accum f acc =
        let rec loop xs acc =
            Wire (Eventive (fun p ->
                let x = invokeEventive p (invokeWire xs)
                match x with
                | WireNil ->
                    WireNil
                | WireCons (a, xs') ->
                    let (acc', b) = invokeEventive p (f acc a)
                    WireCons (b, loop xs' acc')))
        in fun xs -> loop xs acc

    [<CompiledName ("Delay")>]
    let delay x =
        fun xs -> Wire (Eventive (fun p -> WireCons (x, xs)))

    [<CompiledName ("Integ")>]
    let integ init =
        let rec loop (xs: Wire<_>) t0 v0 =
            Wire (Eventive (fun p ->
                let t = p.Time
                let x = invokeEventive p (invokeWire xs)
                match x with
                | WireNil ->
                    WireNil
                | WireCons (a, xs') ->
                    let v = v0 + a * (t - t0)
                    WireCons (v, loop xs' t v)))
        in fun (xs: Wire<_>) ->
            Wire (Eventive (fun p ->
                let t = p.Time
                WireCons (init, loop xs t init)
            ))
 
    [<CompiledName ("IntegChoice")>]
    let integChoice init =
        let rec loop (xs: Wire<_>) t0 v0 =
            Wire (Eventive (fun p ->
                let t = p.Time
                let x = invokeEventive p (invokeWire xs)
                match x with
                | WireNil ->
                    WireNil
                | WireCons (Choice1Of2 v, xs') ->
                    WireCons (v, loop xs' t v)
                | WireCons (Choice2Of2 a, xs') ->
                    let v = v0 + a * (t - t0)
                    WireCons (v, loop xs' t v)))
        in fun (xs: Wire<_>) ->
            Wire (Eventive (fun p ->
                let t = p.Time
                WireCons (init, loop xs t init)
            ))

    [<CompiledName ("DiffSum")>]
    let diffsum (init: float) =
        let rec loop (xs: Wire<_>) t0 v0 =
            Wire (Eventive (fun p ->
                let t = p.Time
                let x = invokeEventive p (invokeWire xs)
                match x with
                | WireNil ->
                    WireNil
                | WireCons (a, xs') ->
                    let v = v0 + a
                    WireCons (v, loop xs' t v)))
        in fun (xs: Wire<_>) ->
            Wire (Eventive (fun p ->
                let t = p.Time
                WireCons (init, loop xs t init)
            ))
 
    [<CompiledName ("DiffSumChoice")>]
    let diffsumChoice (init: float) =
        let rec loop (xs: Wire<_>) t0 v0 =
            Wire (Eventive (fun p ->
                let t = p.Time
                let x = invokeEventive p (invokeWire xs)
                match x with
                | WireNil ->
                    WireNil
                | WireCons (Choice1Of2 v, xs') ->
                    WireCons (v, loop xs' t v)
                | WireCons (Choice2Of2 a, xs') ->
                    let v = v0 + a
                    WireCons (v, loop xs' t v)))
        in fun (xs: Wire<_>) ->
            Wire (Eventive (fun p ->
                let t = p.Time
                WireCons (init, loop xs t init)
            ))
