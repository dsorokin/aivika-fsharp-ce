
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

#nowarn "40"

open System

open Simulation.Aivika
open Simulation.Aivika.Basic

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Processor =

    [<CompiledName ("RoundRobbinUsingIds")>]
    let roundRobbinUsingIds: Processor<Proc<Time * ProcId> * Proc<'a>, 'a> =
        fun xs -> stream {
        
            let! q = InfiniteQueue.createUsingFCFS 
                        |> Simulation.lift
            
            let rec loop = proc {
            
                let! t = InfiniteQueue.dequeue q
                let (x, p) = t
                let! (timeout, pid) = x
                let! result = Proc.timeoutUsingId timeout pid p
                
                match result with
                | Some a -> return a
                | None ->
                    do! InfiniteQueue.enqueue t q
                        |> Eventive.lift
                    return! loop
            } 
            
            let processor = 
                Processor.buffer
                    (Stream.consume <| fun a ->
                        InfiniteQueue.enqueue a q
                            |> Eventive.lift)
                    (Stream.repeat loop)
                
            yield! processor xs
        }

    [<CompiledName ("RoundRobbin")>]
    let roundRobbin: Processor<Proc<Time> * Proc<'a>, 'a> =
        let f (timeout, p) = proc {
            let x = proc {
                let! timeout' = timeout
                let! pid = Proc.createId |> Simulation.lift
                return (timeout', pid)
            }
            return (x, p)
        }
        fun xs -> 
            Stream.mapc f xs |> roundRobbinUsingIds
            
