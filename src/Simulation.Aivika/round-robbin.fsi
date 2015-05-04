
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

namespace Simulation.Aivika.RoundRobbin

open System

open Simulation.Aivika

/// The module contains the Round-Robbin processor.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Processor =

    /// Represents the Round-Robbin processor that tries to perform the input tasks within the specified timeout using the provided process identifiers, where the tasks cancelled by timeout are enqueued to be processed again.
    [<CompiledName ("RoundRobbinUsingIds")>]
    val roundRobbinUsingIds: Processor<Proc<Time * ProcId> * Proc<'a>, 'a>

    /// Represents the Round-Robbin processor that tries to perform the input tasks within the specified timeout, where the tasks cancelled by timeout are enqueued to be processed again.
    [<CompiledName ("RoundRobbin")>]
    val roundRobbin: Processor<Proc<Time> * Proc<'a>, 'a>
