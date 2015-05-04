
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

namespace Simulation.Aivika.Queues

open Simulation.Aivika

/// The module contains additional functions for working with the queues.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Queue =

    /// Returns a simple processor for the queue that just enqueues new items and then dequeues them.
    [<CompiledName ("Processor")>]
    val processor: queue:Queue<'a> -> Processor<'a, 'a>

    /// Returns a processor for the queue that either enqueues new items and then dequeues or loses them.
    [<CompiledName ("ProcessorWithLost")>]
    val processorWithLost: queue:Queue<'a> -> Processor<'a, 'a>

/// The module contains additional functions for working with the infinite queues.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module InfiniteQueue =

    /// Returns a simple processor for the queue that just enqueues new items and then dequeues them.
    [<CompiledName ("Processor")>]
    val processor: queue:InfiniteQueue<'a> -> Processor<'a, 'a>
