
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

/// Represents a mutable reference which state synchronized with the event queue.
[<Sealed>]
type Ref<'a> =

    interface ISignalable<'a>

/// The module contains useful functions for working with the reference.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Ref =

    /// Creates a new reference with the specified initial value.
    [<CompiledName ("Create")>]
    val create: init:'a -> Simulation<Ref<'a>>

    /// Reads the value of the reference.
    [<CompiledName ("Read")>]
    val read: r:Ref<'a> -> Eventive<'a>

    /// Writes a new value into the reference.
    [<CompiledName ("Write")>]
    val write: value:'a -> r:Ref<'a> -> Eventive<unit>

    /// Mutates the contents of the reference.
    [<CompiledName ("Modify")>]
    val modify: f:('a -> 'a) -> r:Ref<'a> -> Eventive<unit>

    /// Increases the reference.
    [<CompiledName ("Inc")>]
    val inc: r:Ref<int> -> Eventive<unit>

    /// Decreases the reference.
    [<CompiledName ("Dec")>]
    val dec: r:Ref<int> -> Eventive<unit>

    /// Returns a signal that notifies about every change of the reference.
    [<CompiledName ("Changed")>]
    val changed: r:Ref<'a> -> Signal<'a>

    /// Returns a signal that notifies about every change of the reference.
    [<CompiledName ("Changed_")>]
    val changed_: r:Ref<'a> -> Signal<unit>
