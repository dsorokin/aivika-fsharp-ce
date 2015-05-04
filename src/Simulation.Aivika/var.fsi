
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

/// Represents a mutable variable which state synchronized with the event queue and that keeps the history of changes.
[<Sealed>]
type Var<'a> =

    interface ISignalable<'a>

/// The module contains useful functions for working with the variable.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Var =

    /// Creates a new variable with the specified initial value.
    [<CompiledName ("Create")>]
    val create: init:'a -> Simulation<Var<'a>>

    /// Reads the recent value of the variable at the current time within a computation (destined to be used within DES).
    [<CompiledName ("Read")>]
    val read: v:Var<'a> -> Eventive<'a>

    /// Reads the first value of the variable at the current time within a computation (destined to be used in ODEs of System Dynamics).
    [<CompiledName ("Memo")>]
    val memo: v:Var<'a> -> Dynamics<'a>

    /// Writes a new value into the variable.
    [<CompiledName ("Write")>]
    val write: value:'a -> v:Var<'a> -> Eventive<unit>

    /// Mutates the contents of the variable.
    [<CompiledName ("Modify")>]
    val modify: f:('a -> 'a) -> v:Var<'a> -> Eventive<unit>

    /// Increases the variable.
    [<CompiledName ("Inc")>]
    val inc: v:Var<int> -> Eventive<unit>

    /// Decreases the variable.
    [<CompiledName ("Dec")>]
    val dec: v:Var<int> -> Eventive<unit>
    
    /// Freezes the variable and returns in arrays the distinct time points and corresponding first and last values which the variable had for each time point.
    [<CompiledName ("Freeze")>]
    val freeze: v:Var<'a> -> Eventive<Time [] * 'a [] * 'a []>

    /// Returns a signal that notifies about every change of the variable state.
    [<CompiledName ("Changed")>]
    val changed: v:Var<'a> -> Signal<'a>

    /// Returns a signal that notifies about every change of the variable state.
    [<CompiledName ("Changed_")>]
    val changed_: v:Var<'a> -> Signal<unit>
