
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

/// Represents a circuit synchronized with the event queue.
type Circuit<'a, 'b> = Wire<'a> -> Wire<'b>

/// The module contains useful functions for working with the circuits.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Circuit =

    /// Runs the circuit using the specified input wire and producing a new output wire.
    [<CompiledName ("Run")>]
    val run: wire:Wire<'a> -> circ:Circuit<'a, 'b> -> Wire<'b>

    /// Creates a new circuit by the specified handling function.
    [<CompiledName ("Arr")>]
    val arr: f:('a -> 'b) -> Circuit<'a, 'b>

    /// Creates a new circuit by the specified handling function that returns the result within the Eventable computation.
    [<CompiledName ("ArrC")>]
    val arrc: f:('a -> Eventive<'b>) -> Circuit<'a, 'b>

    /// Accumulator that outputs a value determined by the supplied function.
    [<CompiledName ("Accum")>]
    val accum: f:('state -> 'a -> Eventive<'state * 'b>) -> state:'state -> Circuit<'a, 'b> 

    /// Delay the input by one step using the specified initial value.
    [<CompiledName ("Delay")>]
    val delay: init:'a -> Circuit<'a, 'a>

    /// An approximation of the integral using Euler's method.
    [<CompiledName ("Integ")>]
    val integ: init:float -> Circuit<float, float>

    /// An approximation of the integral using Euler's method, where we either explicitly set a new integral value or provide the derivative as the second choice.
    [<CompiledName ("IntegChoice")>]
    val integChoice: init:float -> Circuit<Choice<float, float>, float>

    /// A sum for the difference equation.
    [<CompiledName ("DiffSum")>]
    val diffsum: init:float -> Circuit<float, float>

    /// A sum for the difference equation, where we either explicitly set a new sum value or provide the difference as the second choice.
    [<CompiledName ("DiffSumChoice")>]
    val diffsumChoice: init:float -> Circuit<Choice<float, float>, float>
