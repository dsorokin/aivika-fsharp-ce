
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

#nowarn "40"

namespace Simulation.Aivika

open System
open System.Collections.Generic

open Simulation.Aivika.Basic

type internal StreamItem<'a> =
    | StreamNil
    | StreamCons of 'a * Stream<'a>

and [<Sealed; NoEquality; NoComparison>] Stream<'a> internal (p: Proc<StreamItem<'a>>) =

    member internal x.Proc = p

    static member From (m: Parameter<unit>): Stream<'a> =
        Stream (proc {
            do! Parameter.lift m
            return StreamNil
        })

    static member From (m: Simulation<unit>): Stream<'a> =
        Stream (proc {
            do! Simulation.lift m
            return StreamNil
        })

    static member From (m: Dynamics<unit>): Stream<'a> =
        Stream (proc {
            do! Dynamics.lift m
            return StreamNil
        })

    static member From (m: Eventive<unit>): Stream<'a> =
        Stream (proc {
            do! Eventive.lift m
            return StreamNil
        })

    static member From (m: Cont<unit>): Stream<'a> =
        Stream (proc {
            do! Cont.lift m
            return StreamNil
        })
 
    static member From (m: Proc<unit>): Stream<'a> =
        Stream (proc {
            do! m
            return StreamNil
        })

    static member From (m: Stream<'a>) = m

    static member From (m: Wire<'a>): Stream<'a> =
        let rec loop m = Stream (proc {
            let! x = invokeWire m |> Eventive.lift
            match x with
            | WireNil ->
                return StreamNil
            | WireCons (a, m') ->
                return StreamCons (a, loop m')
        })
        loop m

[<AutoOpen>]
module internal StreamInvoke =

    let inline invokeStream (m: Stream<'a>) = m.Proc

module StreamBuilderImpl =

    let zeroS<'a> = 
        Stream (proc.Return (StreamNil): Proc<StreamItem<'a>>)

    let inline yieldS a =
        Stream (proc.Return (StreamCons (a, zeroS)))

    let returnS (u: 'u): Stream<'a> =
        match box u with
        | :? unit -> zeroS
        | _ -> failwithf "Only the unit type is supported."

    let inline bindS m k =
        Stream (proc.Bind (m, fun a -> invokeStream (k a)))

    let inline delayS k =
        Stream (proc.Delay (fun () -> invokeStream (k ())))

    let rec combineS m1 m2 =
        Stream (proc.Bind (invokeStream m1, fun x ->
            match x with
            | StreamNil -> 
                invokeStream m2
            | StreamCons (a, m1') ->
                proc.Return (StreamCons (a, (combineS m1' m2)))))
                
    let rec whileS pred m =
        Stream (proc {
            if pred () then
                return! invokeStream (combineS m (whileS pred m))
            else
                return StreamNil
        })

    let inline usingS (a: 'a when 'a :> IDisposable) k =
        Stream (proc.Using (a, fun a -> invokeStream (k a)))

    let inline tryFinallyS m f =
        Stream (proc.TryFinally (invokeStream m, f))

    let inline tryWithS m k =
        Stream (proc.TryWith (invokeStream m, fun e -> invokeStream (k e)))

    let rec forS es k =
        Stream (proc {
            let! e = invokeStream es
            match e with
            | StreamNil -> 
                return StreamNil
            | StreamCons (a, es') ->
                return! invokeStream (combineS (k a) (forS es' k))
        }) 

    let forSeqS (es: seq<_>) k =
        usingS (es.GetEnumerator()) <| fun ie ->
            let rec loop (ie: IEnumerator<_>) = 
                Stream (proc {
                    if ie.MoveNext () then
                        let a = ie.Current
                        return! invokeStream (combineS (k a) (loop ie))
                    else
                        return StreamNil
                })
            loop ie

open StreamBuilderImpl

[<Sealed>]
type StreamBuilder () =

    member x.Yield (a) = yieldS a
    member x.YieldFrom (m: Stream<_>) = m
    member x.Return (u) = returnS u
    member x.ReturnFrom (m: Proc<_>) = bindS m (fun () -> zeroS)
    member x.Bind (m: Proc<_>, k) = bindS m k
    member x.Delay (k) = delayS k
    member x.Zero () = zeroS
    member x.Combine (m1, m2) = combineS m1 m2
    member x.For (es: Stream<_>, k) = forS es k
    member x.For (es: seq<_>, k) = forSeqS es k
    member x.While (p, m) = whileS p m
    member x.Using (a, k) = usingS a k
    member x.TryFinally (m, f) = tryFinallyS m f
    member x.TryWith (m, k) = tryWithS m k

[<AutoOpen>]
module StreamWorkflow =     

    let stream = StreamBuilder ()

[<AutoOpen>]
module ProcBuilderExtensions =

    type ProcBuilder with

        member x.For (es: Stream<'a>, k: 'a -> Proc<unit>) =
            let rec loop es =
                x.Bind (invokeStream es, fun e ->
                    match e with
                    | StreamNil ->
                        x.Zero ()
                    | StreamCons (a, es') ->
                        x.Combine (k a, loop es')
                )
            loop es

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Stream =

    [<CompiledName ("Lift")>]
    let inline lift (x: Stream<'a>) = (^m: (static member From: Stream<'a> -> ^m) (x))
    
    [<CompiledName ("Map")>]
    let rec map f m =
        Stream (proc {
            let! x = invokeStream m
            match x with
            | StreamNil ->
                return StreamNil
            | StreamCons (a, m') ->
                return StreamCons (f a, map f m')
        })
    
    [<CompiledName ("MapC")>]
    let rec mapc f m =
        Stream (proc {
            let! x = invokeStream m
            match x with
            | StreamNil ->
                return StreamNil
            | StreamCons (a, m') ->
                let! y = f a
                return StreamCons (y, mapc f m')
        })

    [<CompiledName ("Accum")>]
    let accum f acc (m: Stream<_>) =
        stream {
            let r = ref acc
            for a in m do
                let! (acc', b) = f !r a
                r := acc'
                yield b
        }

    [<CompiledName ("Ap")>]
    let rec ap mf m =
        Stream (proc {
            let! f = mf
            let! x = invokeStream m
            match x with
            | StreamNil ->
                return StreamNil
            | StreamCons (a, m') ->
                return StreamCons (f a, ap mf m')
        })
    
    [<CompiledName ("Lift2")>]
    let rec lift2 f m1 m2 =
        Stream (proc {
            let! x1 = invokeStream m1
            let! x2 = invokeStream m2
            match (x1, x2) with
            | (StreamCons (a1, m1'), 
               StreamCons (a2, m2')) ->
                return StreamCons (f a1 a2, lift2 f m1' m2')
            | _ ->
                return StreamNil
        })
    
    [<CompiledName ("Lift3")>]
    let rec lift3 f m1 m2 m3 =
        Stream (proc {
            let! x1 = invokeStream m1
            let! x2 = invokeStream m2
            let! x3 = invokeStream m3
            match (x1, x2, x3) with
            | (StreamCons (a1, m1'), 
               StreamCons (a2, m2'),
               StreamCons (a3, m3')) ->
                return StreamCons (f a1 a2 a3, lift3 f m1' m2' m3')
            | _ ->
                return StreamNil
        })

    [<CompiledName ("Lift4")>]
    let rec lift4 f m1 m2 m3 m4 =
        Stream (proc {
            let! x1 = invokeStream m1
            let! x2 = invokeStream m2
            let! x3 = invokeStream m3
            let! x4 = invokeStream m4
            match (x1, x2, x3, x4) with
            | (StreamCons (a1, m1'), 
               StreamCons (a2, m2'),
               StreamCons (a3, m3'),
               StreamCons (a4, m4')) ->
                return StreamCons (f a1 a2 a3 a4, lift4 f m1' m2' m3' m4')
            | _ ->
                return StreamNil
        })
    
    [<CompiledName ("Lift5")>]
    let rec lift5 f m1 m2 m3 m4 m5 =
        Stream (proc {
            let! x1 = invokeStream m1
            let! x2 = invokeStream m2
            let! x3 = invokeStream m3
            let! x4 = invokeStream m4
            let! x5 = invokeStream m5
            match (x1, x2, x3, x4, x5) with
            | (StreamCons (a1, m1'), 
               StreamCons (a2, m2'),
               StreamCons (a3, m3'),
               StreamCons (a4, m4'),
               StreamCons (a5, m5')) ->
                return StreamCons (f a1 a2 a3 a4 a5, lift5 f m1' m2' m3' m4' m5')
            | _ ->
                return StreamNil
        })

    [<CompiledName ("Zip")>]
    let rec zip m1 m2 =
        Stream (proc {
            let! x1 = invokeStream m1
            let! x2 = invokeStream m2
            match (x1, x2) with
            | (StreamCons (a1, m1'), 
               StreamCons (a2, m2')) ->
                return StreamCons ((a1, a2), zip m1' m2')
            | _ ->
                return StreamNil
        })
    
    [<CompiledName ("Zip3")>]
    let rec zip3 m1 m2 m3 =
        Stream (proc {
            let! x1 = invokeStream m1
            let! x2 = invokeStream m2
            let! x3 = invokeStream m3
            match (x1, x2, x3) with
            | (StreamCons (a1, m1'), 
               StreamCons (a2, m2'),
               StreamCons (a3, m3')) ->
                return StreamCons ((a1, a2, a3), zip3 m1' m2' m3')
            | _ ->
                return StreamNil
        })
        
    [<CompiledName ("OfEnumerable")>]
    let rec ofSeq (ms: seq<_>) = 
        Stream (proc {

            let xs = ResizeArray<_> ()
            let ys = ResizeArray<_> ()

            let rec loop (ix: IEnumerator<_>) =
                if ix.MoveNext () then
                    let x = ix.Current
                    match x with
                    | StreamNil ->
                        StreamNil
                    | StreamCons (a, m) ->
                        xs.Add (a)
                        ys.Add (m)
                        loop ix
                elif xs.Count > 0 then
                    StreamCons (xs :> seq<_>, ofSeq ys)
                else
                    StreamNil

            let loop0 (xs: seq<_>) =
                let ix = xs.GetEnumerator ()
                try
                    loop ix
                finally
                    ix.Dispose ()
                    
            let! xs = Seq.map invokeStream ms |> Proc.ofSeq

            return loop0 xs
        })

    [<CompiledName ("OfList")>]
    let ofList ms = List.toSeq ms |> ofSeq |> map Seq.toList
    
    [<CompiledName ("OfArray")>]
    let ofArray ms = Array.toSeq ms |> ofSeq |> map Seq.toArray

    [<CompiledName ("FromEnumerable")>]
    let fromSeq (es: seq<_>) =
        Stream (proc.Using (es.GetEnumerator(), fun ie ->
            let rec loop (ie: IEnumerator<_>) = proc {
                if ie.MoveNext () then
                    let a = ie.Current
                    return StreamCons (a, Stream (loop ie))
                else
                    return StreamNil
            }
            loop ie)) 

    [<CompiledName ("Memo")>]
    let rec memo m =
        Stream (proc {
                    let! x = invokeStream m
                    match x with
                    | StreamNil ->
                        return StreamNil
                    | StreamCons (a, m') ->
                        return StreamCons (a, memo m')
                } |> Proc.memo)

    [<CompiledName ("ParZip")>]
    let rec parZip m1 m2 =
        Stream (proc {
            let! (x1, x2) = 
                Proc.parZip (invokeStream m1) 
                            (invokeStream m2)
            match (x1, x2) with
            | (StreamCons (a1, m1'), 
               StreamCons (a2, m2')) ->
                return StreamCons ((a1, a2), parZip m1' m2')
            | _ ->
                return StreamNil
        })

    [<CompiledName ("ParZip3")>]
    let rec parZip3 m1 m2 m3 =
        Stream (proc {
            let! (x1, x2, x3) = 
                Proc.parZip3 (invokeStream m1) 
                             (invokeStream m2)
                             (invokeStream m3)
            match (x1, x2, x3) with
            | (StreamCons (a1, m1'), 
               StreamCons (a2, m2'),
               StreamCons (a3, m3')) ->
                return StreamCons ((a1, a2, a3), parZip3 m1' m2' m3')
            | _ ->
                return StreamNil
        })

    [<CompiledName ("Unzip")>]
    let unzip m =
        let m = memo m
        (map fst m, map snd m)

    [<CompiledName ("Filter")>]
    let rec filter p m =
        Stream (proc {
            let! x = invokeStream m
            match x with
            | StreamNil ->
                return StreamNil
            | StreamCons (a, m') ->
                if p a then
                    return StreamCons (a, filter p m')
                else
                    return! invokeStream (filter p m')
        })

    [<CompiledName ("FilterC")>]
    let rec filterc p m =
        Stream (proc {
            let! x = invokeStream m
            match x with
            | StreamNil ->
                return StreamNil
            | StreamCons (a, m') ->
                let! b = p a
                if b then
                    return StreamCons (a, filterc p m')
                else
                    return! invokeStream (filterc p m')
        })

    [<CompiledName ("Concat")>]
    let concat (mm: Stream<_>) = stream {
        for m in mm do
            yield! m
    }

    [<CompiledName ("Append")>]
    let append m1 m2 = stream.Combine (m1, m2)

    [<CompiledName ("Singleton")>]
    let singleton a = stream.Yield (a)

    [<CompiledName ("SingletonC")>]
    let singletonc m = 
        Stream (proc.Bind (m, fun a -> proc.Return (StreamCons (a, stream.Zero ()))))

    [<CompiledName ("Empty")>]
    let empty<'a> = stream.Zero<'a> ()

    [<CompiledName ("Never")>]
    let never<'a> : Stream<'a> = Stream Proc.never

    [<CompiledName ("Repeat")>]
    let rec repeat m =
        Stream (proc.Bind (m, fun a -> proc.Return (StreamCons (a, repeat m))))

    [<CompiledName ("SplitQueueing")>]
    let splitQueueing (strat: #IQueueStrategy) number stream =
        let stream = ref stream
        let r = Resource.create strat 1 |> Simulation.memo
        let rec output = 
            Stream (proc {
                let! r = r |> Simulation.lift
                use! h = Resource.take r
                let! x = invokeStream !stream
                match x with
                | StreamNil ->
                    stream := empty
                    return StreamNil
                | StreamCons (a, m) ->
                    stream := m
                    return StreamCons (a, output)
            })
        List.map (fun i -> output) [1 .. number]

    [<CompiledName ("SplitPrioritising")>]
    let splitPrioritising (strat: #IQueueStrategy) priorities stream =
        let stream = ref stream
        let r = Resource.create strat 1 |> Simulation.memo
        let rec output priority = 
            Stream (proc {
                let! r = r |> Simulation.lift
                let! x = invokeStream priority
                match x with
                | StreamNil ->
                    return StreamNil
                | StreamCons (p, ps) ->
                    use! h = Resource.takeWithPriority p r
                    let! x = invokeStream !stream
                    match x with
                    | StreamNil ->
                        stream := empty
                        return StreamNil
                    | StreamCons (a, m) ->
                        stream := m
                        return StreamCons (a, output ps)
            })
        List.map output priorities

    [<CompiledName ("Split")>]
    let split number stream = splitQueueing QueueStrategy.FCFS number stream

    [<CompiledName ("Split2")>]
    let split2 stream =
        match split 2 stream with
        | [m1; m2] -> (m1, m2)
        | _ -> failwithf "Fatal error."

    [<CompiledName ("Split3")>]
    let split3 stream =
        match split 3 stream with
        | [m1; m2; m3] -> (m1, m2, m3)
        | _ -> failwithf "Fatal error."

    [<CompiledName ("MergeQueueing")>]
    let mergeQueueing (strat: #IQueueStrategy) streams =
        Stream (proc {
            let! reading =
                Resource.createWithMaxCountUsingFCFS 0 (Some 1)
                    |> Simulation.lift
            let! writing =
                Resource.createWithMaxCount strat 1 (Some 1)
                    |> Simulation.lift
            let! conting =
                Resource.createWithMaxCountUsingFCFS 0 (Some 1)
                    |> Simulation.lift
            let r = ref None
            let n = ref <| List.length streams
            let rec writer m = 
                proc {
                    let! x = invokeStream m
                    match x with
                    | StreamNil ->
                        do! Resource.request writing
                        decr n
                        r := None
                        do! Resource.release reading
                        do! Resource.request conting
                    | StreamCons (a, m') ->
                        do! Resource.request writing
                        r := Some a
                        do! Resource.release reading
                        do! Resource.request conting
                        return! writer m'
                }
            let rec reader =
                proc {
                    do! Resource.request reading
                    match !r with
                    | Some a ->
                        r := None
                        do! Resource.release writing
                        return StreamCons (a, 
                            Stream (proc {
                                do! Resource.release conting
                                return! reader
                            }))
                    | None when !n > 0 ->
                        do! Resource.release writing
                        do! Resource.release conting
                        return! reader
                    | None ->
                        do! Resource.release writing
                        do! Resource.release conting
                        return StreamNil
                }
            for stream in streams do
                do! writer stream |> Proc.spawn
            return! reader
        })

    [<CompiledName ("MergePrioritising")>]
    let mergePrioritising (strat: #IQueueStrategy) streams =
        Stream (proc {
            let! reading =
                Resource.createWithMaxCountUsingFCFS 0 (Some 1)
                    |> Simulation.lift
            let! writing =
                Resource.createWithMaxCount strat 1 (Some 1)
                    |> Simulation.lift
            let! conting =
                Resource.createWithMaxCountUsingFCFS 0 (Some 1)
                    |> Simulation.lift
            let r = ref None
            let n = ref <| List.length streams
            let rec writer m = 
                proc {
                    let! x = invokeStream m
                    match x with
                    | StreamNil ->
                        do! Resource.requestWithPriority infinity writing
                        decr n
                        r := None
                        do! Resource.release reading
                        do! Resource.request conting
                    | StreamCons ((priority, a), m') ->
                        do! Resource.requestWithPriority priority writing
                        r := Some a
                        do! Resource.release reading
                        do! Resource.request conting
                        return! writer m'
                }
            let rec reader =
                proc {
                    do! Resource.request reading
                    match !r with
                    | Some a ->
                        r := None
                        do! Resource.release writing
                        return StreamCons (a, 
                            Stream (proc {
                                do! Resource.release conting
                                return! reader
                            }))
                    | None when !n > 0 ->
                        do! Resource.release writing
                        do! Resource.release conting
                        return! reader
                    | None ->
                        do! Resource.release writing
                        do! Resource.release conting
                        return StreamNil
                }
            for stream in streams do
                do! writer stream |> Proc.spawn
            return! reader
        })

    [<CompiledName ("Merge")>]
    let merge streams =
        mergeQueueing QueueStrategy.FCFS streams

    [<CompiledName ("Merge2")>]
    let merge2 m1 m2 = merge [m1; m2]

    [<CompiledName ("Merge3")>]
    let merge3 m1 m2 m3 = merge [m1; m2; m3]

    [<CompiledName ("Consume")>]
    let consume f m =
        let rec loop m =
            proc {
                let! x = invokeStream m
                match x with
                | StreamNil ->
                    return ()
                | StreamCons (a, m') ->
                    do! f a
                    return! loop m'
            }
        loop m

    [<CompiledName ("Sink")>]
    let sink m =
        let rec loop m =
            proc {
                let! x = invokeStream m
                match x with
                | StreamNil ->
                    return ()
                | StreamCons (a, m') ->
                    ignore a
                    return! loop m'
            }
        loop m

    [<CompiledName ("UsingId")>]
    let usingId pid m =
        Stream (invokeStream m |> Proc.usingId pid)

    [<CompiledName ("Par")>]
    let rec par ms =
        Stream (proc {

            let xs = ResizeArray<_> ()
            let ys = ResizeArray<_> ()

            let rec loop zs =
                match zs with
                | [] ->
                    if xs.Count > 0 then
                        StreamCons (xs |> Seq.toList, ys |> Seq.toList |> par)
                    else 
                        StreamNil
                | z :: zs' ->
                    match z with
                    | StreamNil ->
                        StreamNil
                    | StreamCons (a, m) ->
                        xs.Add (a)
                        ys.Add (m)
                        loop zs'

            let! zs = List.map invokeStream ms |> Proc.par

            return loop zs
        })

    [<CompiledName ("Choice1Of2")>]
    let rec choice1Of2 m =
        Stream (proc {
            let! x = invokeStream m
            match x with
            | StreamNil ->
                return StreamNil
            | StreamCons (Choice1Of2 a, m') ->
                return StreamCons (a, choice1Of2 m')
            | StreamCons (Choice2Of2 b, m') ->
                return! invokeStream <| choice1Of2 m'
        })

    [<CompiledName ("Choice2Of2")>]
    let rec choice2Of2 m =
        Stream (proc {
            let! x = invokeStream m
            match x with
            | StreamNil ->
                return StreamNil
            | StreamCons (Choice1Of2 a, m') ->
                return! invokeStream <| choice2Of2 m'
            | StreamCons (Choice2Of2 b, m') ->
                return StreamCons (b, choice2Of2 m')
        })

    [<CompiledName ("Replace1Of2")>]
    let rec replace1Of2 abs cs =
        Stream (proc {
            let! x = invokeStream abs
            match x with
            | StreamNil ->
                return StreamNil
            | StreamCons (Choice1Of2 a, abs') ->
                let! y = invokeStream cs
                match y with
                | StreamNil ->
                    return StreamNil
                | StreamCons (c, cs') ->
                    return StreamCons (Choice1Of2 c, replace1Of2 abs' cs')
            | StreamCons (Choice2Of2 b, abs') ->
                return StreamCons (Choice2Of2 b, replace1Of2 abs' cs)
        }) 

    [<CompiledName ("Replace2Of2")>]
    let rec replace2Of2 abs cs =
        Stream (proc {
            let! x = invokeStream abs
            match x with
            | StreamNil ->
                return StreamNil
            | StreamCons (Choice1Of2 a, abs') ->
                return StreamCons (Choice1Of2 a, replace2Of2 abs' cs)
            | StreamCons (Choice2Of2 b, abs') ->
                let! y = invokeStream cs
                match y with
                | StreamNil ->
                    return StreamNil
                | StreamCons (c, cs') ->
                    return StreamCons (Choice2Of2 c, replace2Of2 abs' cs')
        }) 

    [<CompiledName ("PartitionChoice")>]
    let partitionChoice abs = 
        let abs = memo abs
        (choice1Of2 abs, choice2Of2 abs)

    [<CompiledName ("ToArrival")>]
    let toArrival m =
        let rec loop xs t0 =
            Stream (proc {
                let! t = Dynamics.time |> Dynamics.lift
                let! x = invokeStream xs
                match x with
                | StreamNil ->
                    return StreamNil
                | StreamCons (a, xs') ->
                    let y =
                        match t0 with
                        | None -> { Value = a; Time = t; Delay = None }
                        | Some t0 -> { Value = a; Time = t; Delay = Some (t - t0) }
                    return StreamCons (y, loop xs' (Some t))
            })
        in loop m None
        
    [<CompiledName ("RandomArrival")>]
    let randomArrival p =
        let rec loop t0 =
            Stream (proc {
                match t0 with
                    | None -> ()
                    | Some t0 ->
                        let! t1 = Dynamics.time |> Dynamics.lift
                        if t1 <> t0 then
                            failwithf "The time of requesting for a new arrival is different from the time of the previous arrival."
                let! (delay, a) = Parameter.lift p
                do! Proc.hold delay
                let! t2 = Dynamics.time |> Dynamics.lift
                let arrival = { Value = a; Time = t2; Delay = Some delay }
                return StreamCons (arrival, loop (Some t2))
            })
        loop None

    [<CompiledName ("RandomUniform")>]    
    let randomUniform minimum maximum = 
        randomArrival <|
            parameter.Bind (Parameter.randomUniform minimum maximum,
                fun x -> parameter.Return (x, x))
        
    [<CompiledName ("RandomUniformInt")>]    
    let randomUniformInt minimum maximum = 
        randomArrival <|
            parameter.Bind (Parameter.randomUniformInt minimum maximum,
                fun x -> parameter.Return (float x, x))
        
    [<CompiledName ("RandomNormal")>]    
    let randomNormal mu nu = 
        randomArrival <|
            parameter.Bind (Parameter.randomNormal mu nu,
                fun x -> parameter.Return (x, x))
        
    [<CompiledName ("RandomExponential")>]    
    let randomExponential mu = 
        randomArrival <|
            parameter.Bind (Parameter.randomExponential mu,
                fun x -> parameter.Return (x, x))
    
    [<CompiledName ("RandomErlang")>]    
    let randomErlang beta m = 
        randomArrival <|
            parameter.Bind (Parameter.randomErlang beta m,
                fun x -> parameter.Return (x, x))
    
    [<CompiledName ("RandomPoisson")>]    
    let randomPoisson mu = 
        randomArrival <|
            parameter.Bind (Parameter.randomPoisson mu,
                fun x -> parameter.Return (float x, x))
        
    [<CompiledName ("RandomBinomial")>]    
    let randomBinomial prob trials = 
        randomArrival <|
            parameter.Bind (Parameter.randomBinomial prob trials,
                fun x -> parameter.Return (float x, x))

    [<CompiledName ("Prefetch")>]
    let prefetch m =
        Stream (proc {
            let! reading = 
                    Resource.createWithMaxCountUsingFCFS 0 (Some 1)
                        |> Simulation.lift
            let! writing = 
                    Resource.createWithMaxCountUsingFCFS 1 (Some 1)
                        |> Simulation.lift
            let r = ref None
            let rec writer p = 
                    proc {
                        let! x = invokeStream p
                        match x with
                        | StreamNil ->
                            do! Resource.request writing
                            r := None
                            do! Resource.release reading
                        | StreamCons (a, xs) ->
                            do! Resource.request writing
                            r := Some a
                            do! Resource.release reading
                            return! writer xs
                    }
            let rec reader =
                    proc {
                        do! Resource.request reading
                        let x = !r
                        r := None
                        match x with
                        | None ->
                            do! Resource.release writing
                            return StreamNil
                        | Some a ->
                            do! Resource.release writing
                            return StreamCons (a, Stream reader)
                    }
            do! writer m |> Proc.spawn
            return! reader
        })

    [<CompiledName ("ToSignal")>]
    let toSignal m = 
        proc {
            let! s = SignalSource.create |> Simulation.lift
            let consumer a = SignalSource.trigger a s |> Eventive.lift
            do! consume consumer m |> Proc.spawn
            return SignalSource.publish s
        }

    [<CompiledName ("OfSignal")>]
    let ofSignal s =
        proc {
            let! q  = InfiniteQueue.createUsingFCFS |> Simulation.lift
            let h a = InfiniteQueue.enqueue a q
            let! x  = Signal.subscribe h s |> Eventive.lift
            do! eventive { x.Dispose () } |> Proc.whenCancelling
            return InfiniteQueue.dequeue q |> repeat
        }

    [<CompiledName ("Take")>]
    let rec take n m =
        if n <= 0
        then empty
        else 
            Stream (proc {
                let! x = invokeStream m
                match x with
                | StreamNil -> 
                    return StreamNil
                | StreamCons (a, xs) ->
                    return StreamCons (a, take (n - 1) xs)
            })

    [<CompiledName ("TakeWhile")>]
    let rec takeWhile pred m =
        Stream (proc {
            let! x = invokeStream m
            match x with
            | StreamNil -> 
                return StreamNil
            | StreamCons (a, xs) ->
                let f = pred a
                if f
                then return StreamCons (a, takeWhile pred xs)
                else return StreamNil
        })

    [<CompiledName ("TakeWhileC")>]
    let rec takeWhileC pred m =
        Stream (proc {
            let! x = invokeStream m
            match x with
            | StreamNil -> 
                return StreamNil
            | StreamCons (a, xs) ->
                let! f = pred a
                if f
                then return StreamCons (a, takeWhileC pred xs)
                else return StreamNil
        })

    [<CompiledName ("Drop")>]
    let rec drop n m =
        if n <= 0
        then m
        else 
            Stream (proc {
                let! x = invokeStream m
                match x with
                | StreamNil -> 
                    return StreamNil
                | StreamCons (a, xs) ->
                    return! invokeStream (drop (n - 1) xs)
            })

    [<CompiledName ("DropWhile")>]
    let rec dropWhile pred m =
        Stream (proc {
            let! x = invokeStream m
            match x with
            | StreamNil -> 
                return StreamNil
            | StreamCons (a, xs) ->
                let f = pred a
                if f
                then return! invokeStream (dropWhile pred xs)
                else return StreamCons (a, xs)
        })

    [<CompiledName ("DropWhileC")>]
    let rec dropWhileC pred m =
        Stream (proc {
            let! x = invokeStream m
            match x with
            | StreamNil -> 
                return StreamNil
            | StreamCons (a, xs) ->
                let! f = pred a
                if f
                then return! invokeStream (dropWhileC pred xs)
                else return StreamCons (a, xs)
        })
