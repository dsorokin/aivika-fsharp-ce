
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

open Simulation.Aivika.Collections

type Priority = float

[<Interface>]
type IQueueStorage<'a> =

    abstract IsEmpty: unit -> Eventive<bool>

    abstract Dequeue: unit -> Eventive<'a>

    abstract Enqueue: item:'a -> Eventive<unit>

    abstract Enqueue: priority:float * item:'a -> Eventive<unit>

    abstract DeleteBy: pred:('a -> bool) -> Eventive<'a option>

[<Interface>]
type IQueueStrategy =

    abstract CreateStorage<'a> : unit -> Simulation<IQueueStorage<'a>>

[<Sealed>]
type FCFS () =

    interface IQueueStrategy with

        member x.CreateStorage<'a> () =
            Simulation (fun r ->

                let queue = LinkedList<'a> ()

                { new IQueueStorage<'a> with

                    member x.IsEmpty () =
                        Eventive (fun p -> queue.Count = 0)

                    member x.Dequeue () =
                        Eventive (fun p ->
                            let i = queue.First.Value
                            queue.RemoveFirst ()
                            i)

                    member x.Enqueue (item: 'a) =
                        Eventive (fun p -> 
                            queue.AddLast (item) |> ignore)

                    member x.Enqueue (priority: float, item: 'a) =
                        raise <| NotSupportedException ("The FCFS storage does not support priorities.")

                    member x.DeleteBy (pred) =
                        Eventive (fun p ->
                            let rec loop (n: LinkedListNode<_>) =
                                if n = null then
                                    None
                                elif pred n.Value then
                                    let a = n.Value
                                    queue.Remove (n)
                                    Some a
                                else
                                    loop n.Next
                            loop queue.First)
                })

[<Sealed>]
type LCFS () =

    interface IQueueStrategy with

        member x.CreateStorage<'a> () =
            Simulation (fun r ->

                let queue = LinkedList<'a> ()

                { new IQueueStorage<'a> with

                    member x.IsEmpty () =
                        Eventive (fun p -> queue.Count = 0)

                    member x.Dequeue () =
                        Eventive (fun p ->
                            let i = queue.First.Value
                            queue.RemoveFirst ()
                            i)

                    member x.Enqueue (item: 'a) =
                        Eventive (fun p -> 
                            queue.AddFirst (item) |> ignore)

                    member x.Enqueue (priority: float, item: 'a) =
                        raise <| NotSupportedException ("The LCFS storage does not support priorities.") 

                    member x.DeleteBy (pred) =
                        Eventive (fun p ->
                            let rec loop (n: LinkedListNode<_>) =
                                if n = null then
                                    None
                                elif pred n.Value then
                                    let a = n.Value
                                    queue.Remove (n)
                                    Some a
                                else
                                    loop n.Next
                            loop queue.First)
                })

[<Sealed>]
type SIRO () = 

    interface IQueueStrategy with

        member x.CreateStorage<'a> () =
            Simulation (fun r ->

                let queue = ResizeArray<'a> ()
                let rnd = Random ()

                { new IQueueStorage<'a> with

                    member x.IsEmpty () =
                        Eventive (fun p -> queue.Count = 0)

                    member x.Dequeue () =
                        Eventive (fun p ->

                            let n = rnd.Next (queue.Count)
                            let i = queue.[n]
                            queue.RemoveAt (n)
                            i)

                    member x.Enqueue (item: 'a) =
                        Eventive (fun p -> queue.Add (item))

                    member x.Enqueue (priority: float, item: 'a) =
                        raise <| NotSupportedException ("The SIRO storage does not support priorities.") 

                    member x.DeleteBy (pred) =
                        Eventive (fun p ->
                            let rec loop i =
                                if i >= queue.Count then
                                    None
                                elif pred queue.[i] then
                                    let a = queue.[i]
                                    queue.RemoveAt (i)
                                    Some a
                                else
                                    loop (i + 1)
                            loop 0)
                })

[<Sealed>]
type StaticPriorities () =

    interface IQueueStrategy with

        member x.CreateStorage<'a> () =
            Simulation (fun r ->

                let queue = PriorityQueue<'a> ()

                { new IQueueStorage<'a> with

                    member x.IsEmpty () =
                        Eventive (fun p -> queue.IsEmpty)

                    member x.Dequeue () =
                        Eventive (fun p ->

                            let i = queue.FrontValue
                            queue.Dequeue ()
                            i)

                    member x.Enqueue (item: 'a) =
                        raise <| NotSupportedException ("It supports the static priorities only.")

                    member x.Enqueue (priority: float, item: 'a) =
                        Eventive (fun p -> queue.Enqueue (priority, item))

                    member x.DeleteBy (pred) =
                        Eventive (fun p -> queue.RemoveBy (pred))
                })

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module QueueStrategy =

    [<CompiledName ("FCFS")>]
    let FCFS = FCFS ()

    [<CompiledName ("LCFS")>]
    let LCFS = LCFS ()

    [<CompiledName ("SIRO")>]
    let SIRO = SIRO ()

    [<CompiledName ("StaticPriorities")>]
    let staticPriorities = StaticPriorities ()
