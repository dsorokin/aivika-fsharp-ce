
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

namespace Simulation.Aivika.Results

open System
open System.IO
open System.Globalization
open System.Text

open Simulation.Aivika

type ResultName = string

type ResultDescription = string

type ResultData<'e> = Eventive<'e>

type ResultId = 
    | TimeId
    | SamplingStatsCountId
    | SamplingStatsMinimumId
    | SamplingStatsMaximumId
    | SamplingStatsMeanId
    | SamplingStatsMean2Id
    | SamplingStatsVarianceId
    | SamplingStatsDeviationId
    | TimingStatsCountId
    | TimingStatsMinimumId
    | TimingStatsMaximumId
    | TimingStatsMinimumTimeId
    | TimingStatsMaximumTimeId
    | TimingStatsStartTimeId
    | TimingStatsLastTimeId
    | TimingStatsMeanId
    | TimingStatsVarianceId
    | TimingStatsDeviationId
    | TimingStatsSumId
    | TimingStatsSum2Id
    | QueueInputStrategyId
    | QueueStoringStrategyId
    | QueueOutputStrategyId
    | QueueEmptyId
    | QueueFullId
    | QueueMaxCountId
    | QueueCountId
    | QueueCountStatsId
    | QueueInputCountId
    | QueueLostCountId
    | QueueStoreCountId
    | QueueOutputRequestCountId
    | QueueOutputCountId
    | QueueLoadFactorId
    | QueueInputRateId
    | QueueStoreRateId
    | QueueOutputRequestRateId
    | QueueOutputRateId
    | QueueWaitTimeId
    | QueueTotalWaitTimeId
    | QueueInputWaitTimeId
    | QueueOutputWaitTimeId
    | QueueRateId
    | ServerInitStateId
    | ServerStateId
    | ServerTotalInputWaitTimeId
    | ServerTotalProcessingTimeId
    | ServerTotalOutputWaitTimeId
    | ServerTotalPreemptionTimeId
    | ServerInputWaitTimeId
    | ServerProcessingTimeId
    | ServerOutputWaitTimeId
    | ServerPreemptionTimeId
    | ServerInputWaitFactorId
    | ServerProcessingFactorId
    | ServerOutputWaitFactorId
    | ServerPreemptionFactorId
    | ArrivalProcessingTimeId
    | ResourceCountId
    | ResourceCountStatsId
    | ResourceUtilisationCountId
    | ResourceUtilisationCountStatsId
    | ResourceQueueCountId
    | ResourceQueueCountStatsId
    | ResourceTotalWaitTimeId
    | ResourceWaitTimeId
    | CustomResultId of ResultDescription option

type ResultTypeId = 
    | EmptyId
    | IntId
    | IntListId
    | IntSamplingStatsId
    | IntTimingStatsId
    | DoubleId
    | DoubleListId
    | DoubleSamplingStatsId
    | DoubleTimingStatsId
    | StringId
    | ArrayId of ResultTypeId
    | SeparatorId
    | FiniteQueueId
    | InfiniteQueueId
    | ServerId
    | ArrivalTimerId
    | ResourceId

type [<AbstractClass>] ResultFormatInfo (provider: IFormatProvider) as info = 

    interface IFormatProvider with

        member x.GetFormat (tp) =
            if tp = typeof<ResultId> then box info 
            elif tp = typeof<ResultTypeId> then box info
            else provider.GetFormat (tp)

    abstract GetDescription: id:ResultId -> ResultDescription option
    abstract GetDescription: id:ResultTypeId -> ResultDescription option

    static member CreateInfo (provider) =
        { new ResultFormatInfo (provider) with

            override x.GetDescription (id: ResultId) =
                match id with
                | TimeId                   -> Some "time"
                | SamplingStatsCountId     -> Some "count"
                | SamplingStatsMinimumId   -> Some "minimum"
                | SamplingStatsMaximumId   -> Some "maximum"
                | SamplingStatsMeanId      -> Some "mean"
                | SamplingStatsMean2Id     -> Some "mean square"
                | SamplingStatsVarianceId  -> Some "variance"
                | SamplingStatsDeviationId -> Some "deviation"
                | TimingStatsCountId       -> Some "count"
                | TimingStatsMinimumId     -> Some "minimum"
                | TimingStatsMaximumId     -> Some "maximum"
                | TimingStatsMinimumTimeId -> Some "the time of minimum"
                | TimingStatsMaximumTimeId -> Some "the time of maximum"
                | TimingStatsStartTimeId   -> Some "the start time"
                | TimingStatsLastTimeId    -> Some "the last time"
                | TimingStatsMeanId        -> Some "mean"
                | TimingStatsVarianceId    -> Some "variance"
                | TimingStatsDeviationId   -> Some "deviation"
                | TimingStatsSumId         -> Some "sum"
                | TimingStatsSum2Id        -> Some "sum square"
                | QueueInputStrategyId     -> Some "the enqueueing strategy"
                | QueueStoringStrategyId   -> Some "the storing strategy"
                | QueueOutputStrategyId    -> Some "the dequeueing strategy"
                | QueueEmptyId             -> Some "is the queue empty?"
                | QueueFullId              -> Some "is the queue full?"
                | QueueMaxCountId          -> Some "the queue capacity"
                | QueueCountId             -> Some "the current queue size"
                | QueueCountStatsId        -> Some "the queue size statistics"
                | QueueInputCountId        -> Some "a total number of attempts to enqueue the items"
                | QueueLostCountId         -> Some "a total number of the lost items when trying to enqueue"
                | QueueStoreCountId        -> Some "a total number of the stored items"
                | QueueOutputRequestCountId -> Some "a total number of requests for dequeueing"
                | QueueOutputCountId       -> Some "a total number of the dequeued items"
                | QueueLoadFactorId        -> Some "the queue load (its size divided by its capacity)"
                | QueueInputRateId         -> Some "how many attempts to enqueue per time?"
                | QueueStoreRateId         -> Some "how many items were stored per time?"
                | QueueOutputRequestRateId -> Some "how many requests for dequeueing per time?"
                | QueueOutputRateId        -> Some "how many items were dequeued per time?"
                | QueueWaitTimeId          -> Some "the wait time (stored -> dequeued)"
                | QueueTotalWaitTimeId     -> Some "the total wait time (tried to enqueue -> dequeued)"
                | QueueInputWaitTimeId     -> Some "the enqueue wait time (tried to enqueue -> stored)"
                | QueueOutputWaitTimeId    -> Some "the dequeue wait time (requested for dequeueing -> dequeued)"
                | QueueRateId              -> Some "the average queue rate (= queue size / wait time)"
                | ServerInitStateId        -> Some "the initial state"
                | ServerStateId            -> Some "the current state"
                | ServerTotalInputWaitTimeId  -> Some "the total time spent while waiting for input"
                | ServerTotalProcessingTimeId -> Some "the total time spent on actual processing the tasks"
                | ServerTotalOutputWaitTimeId -> Some "the total time spent on delivering the output"
                | ServerTotalPreemptionTimeId -> Some "the total time spent while being preempted"
                | ServerInputWaitTimeId    -> Some "the time spent while waiting for input"
                | ServerProcessingTimeId   -> Some "the time spent on processing the tasks"
                | ServerOutputWaitTimeId   -> Some "the time spent on delivering the output"
                | ServerPreemptionTimeId   -> Some "the time spent while being preempted"
                | ServerInputWaitFactorId  -> Some "the relative time spent while waiting for input (from 0 to 1)"
                | ServerProcessingFactorId -> Some "the relative time spent on processing the tasks (from 0 to 1)"
                | ServerOutputWaitFactorId -> Some "the relative time spent on delivering the output (from 0 to 1)"
                | ServerPreemptionFactorId -> Some "the relative time spent while being preempted (from 0 to 1)"
                | ArrivalProcessingTimeId  -> Some "the processing time of arrivals"
                | ResourceCountId                 -> Some "the current available count"
                | ResourceCountStatsId            -> Some "the available count statistics"
                | ResourceUtilisationCountId      -> Some "the current utilisation count"
                | ResourceUtilisationCountStatsId -> Some "the utilisation count statistics"
                | ResourceQueueCountId            -> Some "the current queue length"
                | ResourceQueueCountStatsId       -> Some "the queue length statistics"
                | ResourceTotalWaitTimeId         -> Some "the total waiting time"
                | ResourceWaitTimeId              -> Some "the waiting time"
                | CustomResultId z         -> z

            override x.GetDescription (typeId: ResultTypeId) =
                match typeId with
                | EmptyId               -> Some "empty"
                | IntId                 -> Some "integer"
                | IntListId             -> Some "list of integers"
                | IntSamplingStatsId    -> Some "statistics based on integers"
                | IntTimingStatsId      -> Some "timing statistics based on integers"
                | DoubleId              -> Some "floating point number"
                | DoubleListId          -> Some "list of floating point numbers"
                | DoubleSamplingStatsId -> Some "statistics based on floating point numbers"
                | DoubleTimingStatsId   -> Some "timing statistics based on floating point number"
                | StringId              -> Some "string"
                | ArrayId z             -> Some "array"
                | SeparatorId           -> Some "separator"
                | FiniteQueueId         -> Some "finite queue"
                | InfiniteQueueId       -> Some "infinite queue"
                | ServerId              -> Some "server"
                | ArrivalTimerId        -> Some "how long the arrivals are processed?"
                | ResourceId            -> Some "resource"
        }

    static member CurrentInfo = ResultFormatInfo.CreateInfo (CultureInfo.CurrentCulture)
    static member InvariantInfo = ResultFormatInfo.CreateInfo (CultureInfo.InvariantCulture)

type ResultSignal =
    | EmptyResultSignal
    | UnknownResultSignal
    | ResultSignal of Signal<unit>
    | ResultSignalMix of Signal<unit>

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ResultSignal =

    [<CompiledName ("Empty")>]
    let empty: ResultSignal = EmptyResultSignal 

    [<CompiledName ("Merge")>]
    let merge x y =
        match (x, y) with
        | (EmptyResultSignal, z) -> z

        | (UnknownResultSignal, EmptyResultSignal) -> UnknownResultSignal
        | (UnknownResultSignal, UnknownResultSignal) -> UnknownResultSignal
        | (UnknownResultSignal, ResultSignal x) -> ResultSignalMix x
        | (UnknownResultSignal, ((ResultSignalMix x) as z)) -> z

        | ((ResultSignal x) as z, EmptyResultSignal) -> z
        | (ResultSignal x, UnknownResultSignal) -> ResultSignalMix x
        | (ResultSignal x, ResultSignal y) -> ResultSignal (Signal.merge x y)
        | (ResultSignal x, ResultSignalMix y) -> ResultSignalMix (Signal.merge x y)

        | ((ResultSignalMix x) as z, EmptyResultSignal) -> z
        | ((ResultSignalMix x) as z, UnknownResultSignal) -> z
        | (ResultSignalMix x, ResultSignal y) -> ResultSignalMix (Signal.merge x y)
        | (ResultSignalMix x, ResultSignalMix y) -> ResultSignalMix (Signal.merge x y)

    [<CompiledName ("Concat")>]
    let concat xs = List.fold merge EmptyResultSignal xs

    [<CompiledName ("Purify")>]
    let purify (zs: PredefinedSignalSet) x =
        match x with
        | EmptyResultSignal ->
            let z1 = zs.SignalInStartTime |> Signal.map ignore
            let z2 = zs.SignalInStopTime |> Signal.map ignore
            Signal.merge z1 z2
        | UnknownResultSignal ->
            let z0 = zs.SignalInIntegTimes |> Signal.map ignore
            z0
        | ResultSignal x ->
            let z1 = zs.SignalInStartTime |> Signal.map ignore
            let z2 = zs.SignalInStopTime |> Signal.map ignore
            Signal.merge z1 (Signal.merge z2 x)
        | ResultSignalMix x ->
            let z0 = zs.SignalInIntegTimes |> Signal.map ignore
            Signal.merge z0 x

type [<AbstractClass>] ResultValue<'a> () =

    abstract Name: ResultName

    abstract Id: ResultId

    abstract Data: ResultData<'a>
      
    abstract Signal: ResultSignal

    member x.ToContainer () =
       { new ResultContainer<_> () with
            override y.Name   = x.Name
            override y.Id     = x.Id
            override y.Data   = x.Data
            override y.Signal = x.Signal }

    static member FromContainer (container: ResultContainer<ResultData<_>>) =
       { new ResultValue<_> () with
            override y.Name   = container.Name
            override y.Id     = container.Id
            override y.Data   = container.Data
            override y.Signal = container.Signal }

    static member FromData (name: ResultName, comp:ResultData<_>, signal:ResultSignal, id: ResultId) =
        { new ResultValue<_> () with
            override y.Name   = name
            override y.Id     = id
            override y.Data   = comp
            override y.Signal = signal
        }

and [<AbstractClass>] ResultContainer<'a> () =

    abstract Name: ResultName

    abstract Id: ResultId

    abstract Data: 'a

    abstract Signal: ResultSignal

and ResultPair<'a> =
    { Data: ResultData<'a>;
      Signal: ResultSignal }

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ResultValue =

    [<CompiledName ("Map")>]
    let map f (x: ResultValue<_>) =
       { new ResultValue<_> () with
            override y.Name   = x.Name
            override y.Id     = x.Id
            override y.Data   = Eventive.map f x.Data
            override y.Signal = x.Signal }

    [<CompiledName ("Ap")>]
    let ap (f: Eventive<_>) (x: ResultValue<_>) =
        { new ResultValue<_> () with
            override y.Name   = x.Name
            override y.Id     = x.Id
            override y.Data   = Eventive.ap f x.Data
            override y.Signal = x.Signal }

module ResultPairExtensions =

    type Parameter<'a> with

        member x.ToResultPair() =
            { Data = Parameter.lift x; Signal = UnknownResultSignal }       

    type Simulation<'a> with

        member x.ToResultPair() =
            { Data = Simulation.lift x; Signal = UnknownResultSignal }       

    type Dynamics<'a> with

        member x.ToResultPair() =
            { Data = Dynamics.lift x; Signal = UnknownResultSignal }       

    type Eventive<'a> with

        member x.ToResultPair() =
            { Data = x; Signal = UnknownResultSignal }       

    type ISignalable<'a> with

        member x.ToResultPair() =
            { Data = Signalable.read x; Signal = ResultSignal (Signalable.changed_ x) }       

    type Ref<'a> with

        member x.ToResultPair() =
            { Data = Ref.read x; Signal = ResultSignal (Ref.changed_ x) }       

    type Var<'a> with

        member x.ToResultPair() =
            { Data = Var.read x; Signal = ResultSignal (Var.changed_ x) }       

open ResultPairExtensions

module ResultValueExtensions =

    type Parameter<'a> with

        member x.ToResultValue (name: ResultName, id: ResultId) =
            let pair = x.ToResultPair ()
            { new ResultValue<_> () with
                override x.Name   = name
                override x.Id     = id
                override x.Data   = pair.Data
                override x.Signal = pair.Signal }

    type Simulation<'a> with

        member x.ToResultValue (name: ResultName, id: ResultId) =
            let pair = x.ToResultPair ()
            { new ResultValue<_> () with
                override x.Name   = name
                override x.Id     = id
                override x.Data   = pair.Data
                override x.Signal = pair.Signal }

    type Dynamics<'a> with

        member x.ToResultValue (name: ResultName, id: ResultId) =
            let pair = x.ToResultPair ()
            { new ResultValue<_> () with
                override x.Name   = name
                override x.Id     = id
                override x.Data   = pair.Data
                override x.Signal = pair.Signal }

    type Eventive<'a> with

        member x.ToResultValue (name: ResultName, id: ResultId) =
            let pair = x.ToResultPair ()
            { new ResultValue<_> () with
                override x.Name   = name
                override x.Id     = id
                override x.Data   = pair.Data
                override x.Signal = pair.Signal }

    type ISignalable<'a> with

        member x.ToResultValue (name: ResultName, id: ResultId) =
            let pair = x.ToResultPair ()
            { new ResultValue<_> () with
                override x.Name   = name
                override x.Id     = id
                override x.Data   = pair.Data
                override x.Signal = pair.Signal }

    type Ref<'a> with

        member x.ToResultValue (name: ResultName, id: ResultId) =
            let pair = x.ToResultPair ()
            { new ResultValue<_> () with
                override x.Name   = name
                override x.Id     = id
                override x.Data   = pair.Data
                override x.Signal = pair.Signal }

    type Var<'a> with

        member x.ToResultValue (name: ResultName, id: ResultId) =
            let pair = x.ToResultPair ()
            { new ResultValue<_> () with
                override x.Name   = name
                override x.Id     = id
                override x.Data   = pair.Data
                override x.Signal = pair.Signal }

open ResultValueExtensions

type ResultSource =
    | ResultItemSource of ResultItem
    | ResultObjectSource of ResultObject
    | ResultArraySource of ResultArray
    | ResultSeparatorSource of ResultSeparator

and [<AbstractClass>] ResultItem () =

    abstract Name: ResultName

    abstract Id: ResultId

    abstract TypeId: ResultTypeId

    abstract Signal: ResultSignal

    abstract Expand: unit -> ResultSource

    abstract Summary: unit -> ResultSource

    abstract TryGetIntValue: unit -> ResultValue<int> option

    abstract TryGetIntListValue: unit -> ResultValue<int list> option

    abstract TryGetIntStatsValue: unit -> ResultValue<SamplingStats<int>> option

    member x.TryGetIntStatsChoiceValue () =
        let x1 = x.TryGetIntValue ()
        match x1 with
        | Some x1 -> ResultValue.map Choice1Of2 x1 |> Some
        | None ->
            let x2 = x.TryGetIntStatsValue ()
            match x2 with
            | Some x2 -> ResultValue.map Choice2Of2 x2 |> Some
            | None -> None

    abstract TryGetIntTimingStatsValue: unit -> ResultValue<TimingStats<int>> option

    abstract TryGetDoubleValue: unit -> ResultValue<float> option

    abstract TryGetDoubleListValue: unit -> ResultValue<float list> option

    abstract TryGetDoubleStatsValue: unit -> ResultValue<SamplingStats<float>> option

    member x.TryGetDoubleStatsChoiceValue () =
        let x1 = x.TryGetDoubleValue ()
        match x1 with
        | Some x1 -> ResultValue.map Choice1Of2 x1 |> Some
        | None ->
            let x2 = x.TryGetDoubleStatsValue ()
            match x2 with
            | Some x2 -> ResultValue.map Choice2Of2 x2 |> Some
            | None -> None

    abstract TryGetDoubleTimingStatsValue: unit -> ResultValue<TimingStats<float>> option

    abstract TryGetStringValue: unit -> ResultValue<string> option

    abstract TryGetStringValue: provider:IFormatProvider -> ResultValue<string> option

    member x.GetIntValue () = 
        match x.TryGetIntValue () with
        | Some x2 -> x2
        | None -> failwithf "Cannot represent %s as a source of integer values." x.Name

    member x.GetIntListValue () = 
        match x.TryGetIntListValue () with
        | Some x2 -> x2
        | None -> failwithf "Cannot represent %s as a source of lists of integer values." x.Name

    member x.GetIntStatsValue () = 
        match x.TryGetIntStatsValue () with
        | Some x2 -> x2
        | None -> failwithf "Cannot represent %s as a source of statistics based on integer values." x.Name

    member x.GetIntStatsChoiceValue () = 
        match x.TryGetIntStatsChoiceValue () with
        | Some x2 -> x2
        | None -> failwithf "Cannot represent %s as either a source of statistics based on integer values or integer values themselves." x.Name

    member x.GetIntTimingStatsValue () = 
        match x.TryGetIntTimingStatsValue () with
        | Some x2 -> x2
        | None -> failwithf "Cannot represent %s as a source of timing statistics based on integer values." x.Name

    member x.GetDoubleValue () = 
        match x.TryGetDoubleValue () with
        | Some x2 -> x2
        | None -> failwithf "Cannot represent %s as a source of double floating point values." x.Name

    member x.GetDoubleListValue () = 
        match x.TryGetDoubleListValue () with
        | Some x2 -> x2
        | None -> failwithf "Cannot represent %s as a source of lists of double floating point values." x.Name

    member x.GetDoubleStatsValue () = 
        match x.TryGetDoubleStatsValue () with
        | Some x2 -> x2
        | None -> failwithf "Cannot represent %s as a source of statistics based on double floating point values." x.Name

    member x.GetDoubleStatsChoiceValue () = 
        match x.TryGetDoubleStatsChoiceValue () with
        | Some x2 -> x2
        | None -> failwithf "Cannot represent %s as either a source of statistics based on double floaing point values or double values themselves." x.Name

    member x.GetDoubleTimingStatsValue () = 
        match x.TryGetDoubleTimingStatsValue () with
        | Some x2 -> x2
        | None -> failwithf "Cannot represent %s as a source of timing statistics based on double floating point values." x.Name

    member x.GetStringValue () = 
        match x.TryGetStringValue () with
        | Some x2 -> x2
        | None -> failwithf "Cannot represent %s as a source of string values." x.Name

    member x.GetStringValue (provider: IFormatProvider) = 
        match x.TryGetStringValue (provider) with
        | Some x2 -> x2
        | None -> failwithf "Cannot represent %s as a source of string values." x.Name

and [<AbstractClass>] ResultObject () =

    abstract Name: ResultName
    abstract Id: ResultId
    abstract TypeId: ResultTypeId
    abstract Properties: ResultProperty list
    abstract Signal: Lazy<ResultSignal>
    abstract Summary: unit -> ResultSource

and [<AbstractClass>] ResultProperty () =

    abstract Label: ResultName
    abstract Id: ResultId
    abstract Source: ResultSource

and [<AbstractClass>] ResultArray () =

    abstract Name: ResultName
    abstract Id: ResultId
    abstract TypeId: ResultTypeId
    abstract Items: ResultSource []
    abstract Subscript: ResultName []
    abstract Signal: Lazy<ResultSignal>
    abstract Summary: unit -> ResultSource

and [<AbstractClass>] ResultSeparator () =

    abstract Name: ResultName
    abstract Id: ResultId
    abstract TypeId: ResultTypeId

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ResultSource =

    [<CompiledName ("Name")>]
    let name = function
        | ResultItemSource y -> y.Name
        | ResultObjectSource y -> y.Name
        | ResultArraySource y -> y.Name
        | ResultSeparatorSource y -> y.Name

    [<CompiledName ("Id")>]
    let id = function
        | ResultItemSource y -> y.Id
        | ResultObjectSource y -> y.Id
        | ResultArraySource y -> y.Id
        | ResultSeparatorSource y -> y.Id        

    [<CompiledName ("TypeId")>]
    let typeId = function
        | ResultItemSource y -> y.TypeId
        | ResultObjectSource y -> y.TypeId
        | ResultArraySource y -> y.TypeId
        | ResultSeparatorSource y -> y.TypeId        

    [<CompiledName ("Signal")>]
    let signal = function
        | ResultItemSource y -> y.Signal
        | ResultObjectSource y -> y.Signal.Value
        | ResultArraySource y -> y.Signal.Value
        | ResultSeparatorSource y -> EmptyResultSignal

    [<CompiledName ("Summary")>]
    let summary = function
        | ResultItemSource y -> y.Summary ()
        | ResultObjectSource y -> y.Summary ()
        | ResultArraySource y -> y.Summary ()
        | (ResultSeparatorSource y) as x -> x

    [<CompiledName ("Expand")>]
    let rec expand = function
        | ResultItemSource y -> y.Expand ()
        | ResultObjectSource y -> 
            { new ResultObject () with
                override x.Name   = y.Name
                override x.Id     = y.Id
                override x.TypeId = y.TypeId
                override x.Properties =
                    [ for p in y.Properties do
                        yield { new ResultProperty () with
                                    override z.Label  = p.Label
                                    override z.Id     = p.Id
                                    override z.Source = expand p.Source } ]
                override x.Signal = y.Signal
                override x.Summary () = y.Summary ()
            } |> ResultObjectSource
        | ResultArraySource y -> 
            { new ResultArray () with
                override x.Name   = y.Name
                override x.Id     = y.Id
                override x.TypeId = y.TypeId
                override x.Items  =
                    [| for p in y.Items do yield expand p |]
                override x.Subscript = y.Subscript
                override x.Signal = y.Signal
                override x.Summary () = y.Summary ()
            } |> ResultArraySource
        | (ResultSeparatorSource y) as x -> x

    [<CompiledName ("Text")>]
    let text name =
        { new ResultSeparator () with
            override x.Name   = name
            override x.Id     = CustomResultId None
            override x.TypeId = SeparatorId
        } |> ResultSeparatorSource

[<AutoOpen>]
module ResultObjectExtensions =

    type ResultObject with
        member x.Expand () = ResultObjectSource x |> ResultSource.expand

[<AutoOpen>]
module ResultArrayExtensions =

    type ResultArray with
        member x.Expand () = ResultArraySource x |> ResultSource.expand

type BasicResultProperty (label, id, source) =

    inherit ResultProperty ()

    static let name (z: ResultContainer<_>) (label: ResultName) = z.Name + "." + label 

    override x.Label  = label
    override x.Id     = id
    override x.Source = source

    static member FromIntConst (label: ResultName, z: ResultContainer<'a>, f, id: ResultId) =
        let n = name z label
        let z = eventive.Return (f z.Data)
        let z = IntResultItem (n, z, EmptyResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromInt (label: ResultName, z: ResultContainer<'a>, f, id: ResultId) =
        let n = name z label
        let z = f z.Data
        let z = IntResultItem (n, z, UnknownResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromInt (label: ResultName, z: ResultContainer<'a>, f, g, id: ResultId) =
        let n = name z label
        let s = g z.Data |> ResultSignal
        let z = f z.Data
        let z = IntResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromInt (label: ResultName, z: ResultContainer<ResultData<'a>>, f, s: ResultSignal, id: ResultId) =
        let n = name z label
        let z = z.Data |> Eventive.map f
        let z = IntResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromIntListConst (label: ResultName, z: ResultContainer<'a>, f, id: ResultId) =
        let n = name z label
        let z = eventive.Return (f z.Data)
        let z = IntListResultItem (n, z, EmptyResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromIntList (label: ResultName, z: ResultContainer<'a>, f, id: ResultId) =
        let n = name z label
        let z = f z.Data
        let z = IntListResultItem (n, z, UnknownResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromIntList (label: ResultName, z: ResultContainer<'a>, f, g, id: ResultId) =
        let n = name z label
        let s = g z.Data |> ResultSignal
        let z = f z.Data
        let z = IntListResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromIntList (label: ResultName, z: ResultContainer<ResultData<'a>>, f, s: ResultSignal, id: ResultId) =
        let n = name z label
        let z = z.Data |> Eventive.map f
        let z = IntListResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromIntStatsConst (label: ResultName, z: ResultContainer<'a>, f, id: ResultId) =
        let n = name z label
        let z = eventive.Return (f z.Data)
        let z = IntStatsResultItem (n, z, EmptyResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromIntStats (label: ResultName, z: ResultContainer<'a>, f, id: ResultId) =
        let n = name z label
        let z = f z.Data
        let z = IntStatsResultItem (n, z, UnknownResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromIntStats (label: ResultName, z: ResultContainer<'a>, f, g, id: ResultId) =
        let n = name z label
        let s = g z.Data |> ResultSignal
        let z = f z.Data
        let z = IntStatsResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromIntStats (label: ResultName, z: ResultContainer<ResultData<'a>>, f, s: ResultSignal, id: ResultId) =
        let n = name z label
        let z = z.Data |> Eventive.map f
        let z = IntStatsResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromIntTimingStatsConst (label: ResultName, z: ResultContainer<'a>, f, id: ResultId) =
        let n = name z label
        let z = eventive.Return (f z.Data)
        let z = IntTimingStatsResultItem (n, z, EmptyResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromIntTimingStats (label: ResultName, z: ResultContainer<'a>, f, id: ResultId) =
        let n = name z label
        let z = f z.Data
        let z = IntTimingStatsResultItem (n, z, UnknownResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromIntTimingStats (label: ResultName, z: ResultContainer<'a>, f, g, id: ResultId) =
        let n = name z label
        let s = g z.Data |> ResultSignal
        let z = f z.Data
        let z = IntTimingStatsResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromIntTimingStats (label: ResultName, z: ResultContainer<ResultData<'a>>, f, s: ResultSignal, id: ResultId) =
        let n = name z label
        let z = z.Data |> Eventive.map f
        let z = IntTimingStatsResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromDoubleConst (label: ResultName, z: ResultContainer<'a>, f, id: ResultId) =
        let n = name z label
        let z = eventive.Return (f z.Data)
        let z = DoubleResultItem (n, z, EmptyResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromDouble (label: ResultName, z: ResultContainer<'a>, f, id: ResultId) =
        let n = name z label
        let z = f z.Data
        let z = DoubleResultItem (n, z, UnknownResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromDouble (label: ResultName, z: ResultContainer<'a>, f, g, id: ResultId) =
        let n = name z label
        let s = g z.Data |> ResultSignal
        let z = f z.Data
        let z = DoubleResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromDouble (label: ResultName, z: ResultContainer<ResultData<'a>>, f, s: ResultSignal, id: ResultId) =
        let n = name z label
        let z = z.Data |> Eventive.map f
        let z = DoubleResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromDoubleListConst (label: ResultName, z: ResultContainer<'a>, f, id: ResultId) =
        let n = name z label
        let z = eventive.Return (f z.Data)
        let z = DoubleListResultItem (n, z, EmptyResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromDoubleList (label: ResultName, z: ResultContainer<'a>, f, id: ResultId) =
        let n = name z label
        let z = f z.Data
        let z = DoubleListResultItem (n, z, UnknownResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromDoubleList (label: ResultName, z: ResultContainer<'a>, f, g, id: ResultId) =
        let n = name z label
        let s = g z.Data |> ResultSignal
        let z = f z.Data
        let z = DoubleListResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromDoubleList (label: ResultName, z: ResultContainer<ResultData<'a>>, f, s: ResultSignal, id: ResultId) =
        let n = name z label
        let z = z.Data |> Eventive.map f
        let z = DoubleListResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromDoubleStatsConst (label: ResultName, z: ResultContainer<'a>, f, id: ResultId) =
        let n = name z label
        let z = eventive.Return (f z.Data)
        let z = DoubleStatsResultItem (n, z, EmptyResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromDoubleStats (label: ResultName, z: ResultContainer<'a>, f, id: ResultId) =
        let n = name z label
        let z = f z.Data
        let z = DoubleStatsResultItem (n, z, UnknownResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromDoubleStats (label: ResultName, z: ResultContainer<'a>, f, g, id: ResultId) =
        let n = name z label
        let s = g z.Data |> ResultSignal
        let z = f z.Data
        let z = DoubleStatsResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromDoubleStats (label: ResultName, z: ResultContainer<ResultData<'a>>, f, s: ResultSignal, id: ResultId) =
        let n = name z label
        let z = z.Data |> Eventive.map f
        let z = DoubleStatsResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromDoubleTimingStatsConst (label: ResultName, z: ResultContainer<'a>, f, id: ResultId) =
        let n = name z label
        let z = eventive.Return (f z.Data)
        let z = DoubleTimingStatsResultItem (n, z, EmptyResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromDoubleTimingStats (label: ResultName, z: ResultContainer<'a>, f, id: ResultId) =
        let n = name z label
        let z = f z.Data
        let z = DoubleTimingStatsResultItem (n, z, UnknownResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromDoubleTimingStats (label: ResultName, z: ResultContainer<'a>, f, g, id: ResultId) =
        let n = name z label
        let s = g z.Data |> ResultSignal
        let z = f z.Data
        let z = DoubleTimingStatsResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromDoubleTimingStats (label: ResultName, z: ResultContainer<ResultData<'a>>, f, s: ResultSignal, id: ResultId) =
        let n = name z label
        let z = z.Data |> Eventive.map f
        let z = DoubleTimingStatsResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromStringConst (label: ResultName, z: ResultContainer<'a>, f, id: ResultId) =
        let n = name z label
        let z = eventive.Return (f z.Data)
        let z = StringResultItem (n, z, EmptyResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromString (label: ResultName, z: ResultContainer<'a>, f, id: ResultId) =
        let n = name z label
        let z = f z.Data
        let z = StringResultItem (n, z, UnknownResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromString (label: ResultName, z: ResultContainer<'a>, f, g, id: ResultId) =
        let n = name z label
        let s = g z.Data |> ResultSignal
        let z = f z.Data
        let z = StringResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromString (label: ResultName, z: ResultContainer<ResultData<'a>>, f, s: ResultSignal, id: ResultId) =
        let n = name z label
        let z = z.Data |> Eventive.map f
        let z = StringResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromBooleanConst (label: ResultName, z: ResultContainer<'a>, f: 'a -> bool, id: ResultId) =
        let n = name z label
        let z = eventive.Return ((f z.Data) |> string)
        let z = StringResultItem (n, z, EmptyResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromBoolean (label: ResultName, z: ResultContainer<'a>, f: 'a -> Eventive<bool>, id: ResultId) =
        let n = name z label
        let z = f z.Data |> Eventive.map string
        let z = StringResultItem (n, z, UnknownResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromBoolean (label: ResultName, z: ResultContainer<'a>, f: 'a -> Eventive<bool>, g, id: ResultId) =
        let n = name z label
        let s = g z.Data |> ResultSignal
        let z = f z.Data |> Eventive.map string
        let z = StringResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromBoolean (label: ResultName, z: ResultContainer<ResultData<'a>>, f: 'a -> bool, s: ResultSignal, id: ResultId) =
        let n = name z label
        let z = z.Data |> Eventive.map (f >> string)
        let z = StringResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromAnyConst (label: ResultName, z: ResultContainer<'a>, f: 'a -> 'b, id: ResultId) =
        let n = name z label
        let z = eventive.Return ((f z.Data) |> box |> string)
        let z = StringResultItem (n, z, EmptyResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromAny (label: ResultName, z: ResultContainer<'a>, f: 'a -> Eventive<'b>, id: ResultId) =
        let n = name z label
        let z = f z.Data |> Eventive.map (box >> string)
        let z = StringResultItem (n, z, UnknownResultSignal, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromAny (label: ResultName, z: ResultContainer<'a>, f: 'a -> Eventive<'b>, g, id: ResultId) =
        let n = name z label
        let s = g z.Data |> ResultSignal
        let z = f z.Data |> Eventive.map (box >> string)
        let z = StringResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

    static member FromAny (label: ResultName, z: ResultContainer<ResultData<'a>>, f: 'a -> 'b, s: ResultSignal, id: ResultId) =
        let n = name z label
        let z = z.Data |> Eventive.map (f >> box >> string)
        let z = StringResultItem (n, z, s, id) :> ResultItem |> ResultItemSource
        BasicResultProperty (label, id, z) :> ResultProperty

and IntResultItem (v: ResultValue<int>) =

    inherit ResultItem ()

    new (name: ResultName, num, id: ResultId) =
        IntResultItem (ResultValue<_>.FromData (name, eventive.Return (num), EmptyResultSignal, id))

    new (name: ResultName, comp, id: ResultId) =
        IntResultItem (ResultValue<_>.FromData (name, comp, UnknownResultSignal, id))

    new (name: ResultName, comp, signal, id: ResultId) =
        IntResultItem (ResultValue<_>.FromData (name, comp, signal, id))

    override x.Name   = v.Name
    override x.Id     = v.Id
    override x.TypeId = IntId
    override x.Signal = v.Signal

    override x.Expand ()  = ResultItemSource x
    override x.Summary () = ResultItemSource x

    override x.TryGetIntValue () = v |> Some
    override x.TryGetIntListValue() = ResultValue.map (fun a -> [a]) v |> Some
    override x.TryGetIntStatsValue() = ResultValue.map (fun a -> SamplingStats.add a SamplingStats.emptyInts) v |> Some
    override x.TryGetIntTimingStatsValue() = None

    override x.TryGetDoubleValue () = ResultValue.map float v |> Some
    override x.TryGetDoubleListValue() = ResultValue.map (fun a -> [float a]) v |> Some
    override x.TryGetDoubleStatsValue() = ResultValue.map (fun a -> SamplingStats.add (float a) SamplingStats.emptyFloats) v |> Some
    override x.TryGetDoubleTimingStatsValue() = None

    override x.TryGetStringValue () = ResultValue.map (fun a -> a.ToString ()) v |> Some
    override x.TryGetStringValue (provider) = ResultValue.map (fun (a: int) -> Convert.ToString (a, provider)) v |> Some

and IntListResultItem (v: ResultValue<int list>) =

    inherit ResultItem ()

    new (name: ResultName, num, id: ResultId) =
        IntListResultItem (ResultValue<_>.FromData (name, eventive.Return (num), EmptyResultSignal, id))

    new (name: ResultName, comp, id: ResultId) =
        IntListResultItem (ResultValue<_>.FromData (name, comp, UnknownResultSignal, id))

    new (name: ResultName, comp, signal, id: ResultId) =
        IntListResultItem (ResultValue<_>.FromData (name, comp, signal, id))

    override x.Name   = v.Name
    override x.Id     = v.Id
    override x.TypeId = IntListId
    override x.Signal = v.Signal

    override x.Expand ()  = ResultItemSource x
    override x.Summary () = ResultItemSource x

    override x.TryGetIntValue () = None
    override x.TryGetIntListValue() = v |> Some
    override x.TryGetIntStatsValue() = ResultValue.map (fun a -> SamplingStats.emptyInts |> SamplingStats.appendSeq a) v |> Some
    override x.TryGetIntTimingStatsValue() = None

    override x.TryGetDoubleValue () = None
    override x.TryGetDoubleListValue() = ResultValue.map (fun a -> List.map float a) v |> Some
    override x.TryGetDoubleStatsValue() = ResultValue.map (fun a -> SamplingStats.emptyInts |> SamplingStats.appendSeq a |> SamplingStats.fromIntsToFloats) v |> Some
    override x.TryGetDoubleTimingStatsValue() = None

    override x.TryGetStringValue () = ResultValue.map (fun a -> a.ToString ()) v |> Some
    override x.TryGetStringValue (provider: IFormatProvider) = 
        ResultValue.map (fun a -> 

            let b = StringBuilder()
            b.Append ('[') |> ignore

            a |> List.iteri (fun i (x: int) -> 
                if i > 0 then b.Append ("; ") |> ignore
                b.Append (Convert.ToString (x, provider)) |> ignore)
            
            b.Append (']') |> ignore
            b.ToString ()) v |> Some

and IntStatsResultItem (v: ResultValue<SamplingStats<int>>) =

    inherit ResultItem ()

    new (name: ResultName, num, id: ResultId) =
        IntStatsResultItem (ResultValue<_>.FromData (name, eventive.Return (num), EmptyResultSignal, id))

    new (name: ResultName, comp, id: ResultId) =
        IntStatsResultItem (ResultValue<_>.FromData (name, comp, UnknownResultSignal, id))

    new (name: ResultName, comp, signal, id: ResultId) =
        IntStatsResultItem (ResultValue<_>.FromData (name, comp, signal, id))

    override x.Name   = v.Name
    override x.Id     = v.Id
    override x.TypeId = IntSamplingStatsId
    override x.Signal = v.Signal

    override x.Expand ()  = IntStatsResultObject (v) :> ResultObject |> ResultObjectSource
    override x.Summary () = StringResultItem (v |> ResultValue.map string) :> ResultItem |> ResultItemSource

    override x.TryGetIntValue () = None
    override x.TryGetIntListValue() = None
    override x.TryGetIntStatsValue() = v |> Some
    override x.TryGetIntTimingStatsValue() = None

    override x.TryGetDoubleValue () = None
    override x.TryGetDoubleListValue() = None
    override x.TryGetDoubleStatsValue() = ResultValue.map SamplingStats.fromIntsToFloats v |> Some
    override x.TryGetDoubleTimingStatsValue() = None

    override x.TryGetStringValue () = ResultValue.map (fun a -> a.ToString ()) v |> Some
    override x.TryGetStringValue (provider) = ResultValue.map (fun (a: SamplingStats<int>) -> a.ToString (provider)) v |> Some

and IntTimingStatsResultItem (v: ResultValue<TimingStats<int>>) =

    inherit ResultItem ()

    let normalise = 
        eventive {
            let! n = Dynamics.integIteration |> Dynamics.lift
            let g x = TimingStats.normalise n x
            return g
        }

    new (name: ResultName, num, id: ResultId) =
        IntTimingStatsResultItem (ResultValue<_>.FromData (name, eventive.Return (num), EmptyResultSignal, id))

    new (name: ResultName, comp, id: ResultId) =
        IntTimingStatsResultItem (ResultValue<_>.FromData (name, comp, UnknownResultSignal, id))

    new (name: ResultName, comp, signal, id: ResultId) =
        IntTimingStatsResultItem (ResultValue<_>.FromData (name, comp, signal, id))

    override x.Name   = v.Name
    override x.Id     = v.Id
    override x.TypeId = IntTimingStatsId
    override x.Signal = v.Signal

    override x.Expand ()  = IntTimingStatsResultObject (v) :> ResultObject |> ResultObjectSource
    override x.Summary () = StringResultItem (v |> ResultValue.map string) :> ResultItem |> ResultItemSource

    override x.TryGetIntValue () = None
    override x.TryGetIntListValue() = None
    override x.TryGetIntStatsValue() = ResultValue.ap normalise v |> Some 
    override x.TryGetIntTimingStatsValue() = v |> Some

    override x.TryGetDoubleValue () = None
    override x.TryGetDoubleListValue() = None
    override x.TryGetDoubleStatsValue() = ResultValue.ap normalise v |> ResultValue.map SamplingStats.fromIntsToFloats |> Some
    override x.TryGetDoubleTimingStatsValue() = ResultValue.map TimingStats.fromIntsToFloats v |> Some

    override x.TryGetStringValue () = ResultValue.map (fun a -> a.ToString ()) v |> Some
    override x.TryGetStringValue (provider) = ResultValue.map (fun (a: TimingStats<int>) -> a.ToString (provider)) v |> Some

and DoubleResultItem (v: ResultValue<float>) =

    inherit ResultItem ()

    new (name: ResultName, num, id: ResultId) =
        DoubleResultItem (ResultValue<_>.FromData (name, eventive.Return (num), EmptyResultSignal, id))

    new (name: ResultName, comp, id: ResultId) =
        DoubleResultItem (ResultValue<_>.FromData (name, comp, UnknownResultSignal, id))

    new (name: ResultName, comp, signal, id: ResultId) =
        DoubleResultItem (ResultValue<_>.FromData (name, comp, signal, id))

    override x.Name   = v.Name
    override x.Id     = v.Id
    override x.TypeId = DoubleId
    override x.Signal = v.Signal

    override x.Expand ()  = ResultItemSource x
    override x.Summary () = ResultItemSource x

    override x.TryGetIntValue () = None
    override x.TryGetIntListValue() = None
    override x.TryGetIntStatsValue() = None
    override x.TryGetIntTimingStatsValue() = None

    override x.TryGetDoubleValue () = v |> Some
    override x.TryGetDoubleListValue() = ResultValue.map (fun a -> [a]) v |> Some
    override x.TryGetDoubleStatsValue() = ResultValue.map (fun a -> SamplingStats.add a SamplingStats.emptyFloats) v |> Some
    override x.TryGetDoubleTimingStatsValue() = None

    override x.TryGetStringValue () = ResultValue.map (fun a -> a.ToString ()) v |> Some
    override x.TryGetStringValue (provider) = ResultValue.map (fun (a: float) -> Convert.ToString (a, provider)) v |> Some

and DoubleListResultItem (v: ResultValue<float list>) =

    inherit ResultItem ()

    new (name: ResultName, num, id: ResultId) =
        DoubleListResultItem (ResultValue<_>.FromData (name, eventive.Return (num), EmptyResultSignal, id))

    new (name: ResultName, comp, id: ResultId) =
        DoubleListResultItem (ResultValue<_>.FromData (name, comp, UnknownResultSignal, id))

    new (name: ResultName, comp, signal, id: ResultId) =
        DoubleListResultItem (ResultValue<_>.FromData (name, comp, signal, id))

    override x.Name   = v.Name
    override x.Id     = v.Id
    override x.TypeId = DoubleListId
    override x.Signal = v.Signal

    override x.Expand ()  = ResultItemSource x
    override x.Summary () = ResultItemSource x

    override x.TryGetIntValue () = None
    override x.TryGetIntListValue() = None
    override x.TryGetIntStatsValue() = None
    override x.TryGetIntTimingStatsValue() = None

    override x.TryGetDoubleValue () = None
    override x.TryGetDoubleListValue() = v |> Some
    override x.TryGetDoubleStatsValue() = ResultValue.map (fun a -> SamplingStats.emptyFloats |> SamplingStats.appendSeq a) v |> Some
    override x.TryGetDoubleTimingStatsValue() = None

    override x.TryGetStringValue () = ResultValue.map (fun a -> a.ToString ()) v |> Some
    override x.TryGetStringValue (provider: IFormatProvider) = 
        ResultValue.map (fun a -> 

            let b = StringBuilder()
            b.Append ('[') |> ignore

            a |> List.iteri (fun i (x: float) -> 
                if i > 0 then b.Append ("; ") |> ignore
                b.Append (Convert.ToString (x, provider)) |> ignore)
            
            b.Append (']') |> ignore
            b.ToString ()) v |> Some

and DoubleStatsResultItem (v: ResultValue<SamplingStats<float>>) =

    inherit ResultItem ()

    new (name: ResultName, num, id: ResultId) =
        DoubleStatsResultItem (ResultValue<_>.FromData (name, eventive.Return (num), EmptyResultSignal, id))

    new (name: ResultName, comp, id: ResultId) =
        DoubleStatsResultItem (ResultValue<_>.FromData (name, comp, UnknownResultSignal, id))

    new (name: ResultName, comp, signal, id: ResultId) =
        DoubleStatsResultItem (ResultValue<_>.FromData (name, comp, signal, id))

    override x.Name   = v.Name
    override x.Id     = v.Id
    override x.TypeId = DoubleSamplingStatsId
    override x.Signal = v.Signal

    override x.Expand ()  = DoubleStatsResultObject (v) :> ResultObject |> ResultObjectSource
    override x.Summary () = StringResultItem (v |> ResultValue.map string) :> ResultItem |> ResultItemSource

    override x.TryGetIntValue () = None
    override x.TryGetIntListValue() = None
    override x.TryGetIntStatsValue() = None
    override x.TryGetIntTimingStatsValue() = None

    override x.TryGetDoubleValue () = None
    override x.TryGetDoubleListValue() = None
    override x.TryGetDoubleStatsValue() = v |> Some
    override x.TryGetDoubleTimingStatsValue() = None

    override x.TryGetStringValue () = ResultValue.map (fun a -> a.ToString ()) v |> Some
    override x.TryGetStringValue (provider) = ResultValue.map (fun (a: SamplingStats<float>) -> a.ToString (provider)) v |> Some

and DoubleTimingStatsResultItem (v: ResultValue<TimingStats<float>>) =

    inherit ResultItem ()

    let normalise = 
        eventive {
            let! n = Dynamics.integIteration |> Dynamics.lift
            let g x = TimingStats.normalise n x
            return g
        }

    new (name: ResultName, num, id: ResultId) =
        DoubleTimingStatsResultItem (ResultValue<_>.FromData (name, eventive.Return (num), EmptyResultSignal, id))

    new (name: ResultName, comp, id: ResultId) =
        DoubleTimingStatsResultItem (ResultValue<_>.FromData (name, comp, UnknownResultSignal, id))

    new (name: ResultName, comp, signal, id: ResultId) =
        DoubleTimingStatsResultItem (ResultValue<_>.FromData (name, comp, signal, id))

    override x.Name   = v.Name
    override x.Id     = v.Id
    override x.TypeId = DoubleTimingStatsId
    override x.Signal = v.Signal

    override x.Expand ()  = DoubleTimingStatsResultObject (v) :> ResultObject |> ResultObjectSource
    override x.Summary () = StringResultItem (v |> ResultValue.map string) :> ResultItem |> ResultItemSource

    override x.TryGetIntValue () = None
    override x.TryGetIntListValue() = None
    override x.TryGetIntStatsValue() = None
    override x.TryGetIntTimingStatsValue() = None

    override x.TryGetDoubleValue () = None
    override x.TryGetDoubleListValue() = None
    override x.TryGetDoubleStatsValue() = ResultValue.ap normalise v |> Some
    override x.TryGetDoubleTimingStatsValue() = v |> Some

    override x.TryGetStringValue () = ResultValue.map (fun a -> a.ToString ()) v |> Some
    override x.TryGetStringValue (provider) = ResultValue.map (fun (a: TimingStats<float>) -> a.ToString (provider)) v |> Some

and StringResultItem (v: ResultValue<string>) =

    inherit ResultItem ()

    new (name: ResultName, num, id: ResultId) =
        StringResultItem (ResultValue<_>.FromData (name, eventive.Return (num), EmptyResultSignal, id))

    new (name: ResultName, comp, id: ResultId) =
        StringResultItem (ResultValue<_>.FromData (name, comp, UnknownResultSignal, id))

    new (name: ResultName, comp, signal, id: ResultId) =
        StringResultItem (ResultValue<_>.FromData (name, comp, signal, id))

    override x.Name   = v.Name
    override x.Id     = v.Id
    override x.TypeId = StringId
    override x.Signal = v.Signal

    override x.Expand ()  = ResultItemSource x
    override x.Summary () = ResultItemSource x

    override x.TryGetIntValue () = None
    override x.TryGetIntListValue() = None
    override x.TryGetIntStatsValue() = None
    override x.TryGetIntTimingStatsValue() = None

    override x.TryGetDoubleValue () = None
    override x.TryGetDoubleListValue() = None
    override x.TryGetDoubleStatsValue() = None
    override x.TryGetDoubleTimingStatsValue() = None

    override x.TryGetStringValue () = v |> Some
    override x.TryGetStringValue (provider) = v |> Some

and EmptyResultItem (name, id) =

    inherit ResultItem ()

    override x.Name   = name
    override x.Id     = id
    override x.TypeId = EmptyId
    override x.Signal = EmptyResultSignal

    override x.Expand ()  = ResultItemSource x
    override x.Summary () = ResultItemSource x

    override x.TryGetIntValue () = None
    override x.TryGetIntListValue() = None
    override x.TryGetIntStatsValue() = None
    override x.TryGetIntTimingStatsValue() = None

    override x.TryGetDoubleValue () = None
    override x.TryGetDoubleListValue() = None
    override x.TryGetDoubleStatsValue() = None
    override x.TryGetDoubleTimingStatsValue() = None

    override x.TryGetStringValue () = None
    override x.TryGetStringValue (provider) = None

and IntStatsResultObject (v: ResultValue<SamplingStats<_>>) =

    inherit ResultObject ()

    override x.Name       = v.Name
    override x.Id         = v.Id
    override x.TypeId     = IntSamplingStatsId
    override x.Signal     = lazy v.Signal
    override x.Summary () = StringResultItem (v |> ResultValue.map string) :> ResultItem |> ResultItemSource 
    override x.Properties =
        let z = v.ToContainer ()
        let s = v.Signal
        [ BasicResultProperty.FromInt ("count", z, SamplingStats.count, s, SamplingStatsCountId);
          BasicResultProperty.FromDouble ("mean", z, SamplingStats.mean, s, SamplingStatsMeanId);
          BasicResultProperty.FromDouble ("mean2", z, SamplingStats.mean2, s, SamplingStatsMean2Id);
          BasicResultProperty.FromDouble ("std", z, SamplingStats.deviation, s, SamplingStatsDeviationId);
          BasicResultProperty.FromDouble ("var", z, SamplingStats.variance, s, SamplingStatsVarianceId);
          BasicResultProperty.FromInt ("min", z, SamplingStats.minimum, s, SamplingStatsMinimumId);
          BasicResultProperty.FromInt ("max", z, SamplingStats.maximum, s, SamplingStatsMaximumId) ]

and DoubleStatsResultObject (v: ResultValue<SamplingStats<_>>) =

    inherit ResultObject ()

    override x.Name       = v.Name
    override x.Id         = v.Id
    override x.TypeId     = DoubleSamplingStatsId
    override x.Signal     = lazy v.Signal
    override x.Summary () = StringResultItem (v |> ResultValue.map string) :> ResultItem |> ResultItemSource 
    override x.Properties =
        let z = v.ToContainer ()
        let s = v.Signal
        [ BasicResultProperty.FromInt ("count", z, SamplingStats.count, s, SamplingStatsCountId);
          BasicResultProperty.FromDouble ("mean", z, SamplingStats.mean, s, SamplingStatsMeanId);
          BasicResultProperty.FromDouble ("mean2", z, SamplingStats.mean2, s, SamplingStatsMean2Id);
          BasicResultProperty.FromDouble ("std", z, SamplingStats.deviation, s, SamplingStatsDeviationId);
          BasicResultProperty.FromDouble ("var", z, SamplingStats.variance, s, SamplingStatsVarianceId);
          BasicResultProperty.FromDouble ("min", z, SamplingStats.minimum, s, SamplingStatsMinimumId);
          BasicResultProperty.FromDouble ("max", z, SamplingStats.maximum, s, SamplingStatsMaximumId) ]

and IntTimingStatsResultObject (v: ResultValue<TimingStats<_>>) =

    inherit ResultObject ()

    override x.Name       = v.Name
    override x.Id         = v.Id
    override x.TypeId     = IntTimingStatsId
    override x.Signal     = lazy v.Signal
    override x.Summary () = StringResultItem (v |> ResultValue.map string) :> ResultItem |> ResultItemSource 
    override x.Properties =
        let z = v.ToContainer ()
        let s = v.Signal
        [ BasicResultProperty.FromInt ("count", z, TimingStats.count, s, TimingStatsCountId);
          BasicResultProperty.FromDouble ("mean", z, TimingStats.mean, s, TimingStatsMeanId);
          BasicResultProperty.FromDouble ("std", z, TimingStats.deviation, s, TimingStatsDeviationId);
          BasicResultProperty.FromDouble ("var", z, TimingStats.variance, s, TimingStatsVarianceId);
          BasicResultProperty.FromInt ("min", z, TimingStats.minimum, s, TimingStatsMinimumId);
          BasicResultProperty.FromInt ("max", z, TimingStats.maximum, s, TimingStatsMaximumId);
          BasicResultProperty.FromDouble ("minTime", z, TimingStats.minimumTime, s, TimingStatsMinimumTimeId);
          BasicResultProperty.FromDouble ("maxTime", z, TimingStats.maximumTime, s, TimingStatsMaximumTimeId);
          BasicResultProperty.FromDouble ("startTime", z, TimingStats.startTime, s, TimingStatsStartTimeId);
          BasicResultProperty.FromDouble ("lastTime", z, TimingStats.lastTime, s, TimingStatsLastTimeId);
          BasicResultProperty.FromDouble ("sum", z, TimingStats.sum, s, TimingStatsSumId);
          BasicResultProperty.FromDouble ("sum2", z, TimingStats.sum2, s, TimingStatsSum2Id) ]

and DoubleTimingStatsResultObject (v: ResultValue<TimingStats<_>>) =

    inherit ResultObject ()

    override x.Name       = v.Name
    override x.Id         = v.Id
    override x.TypeId     = DoubleTimingStatsId
    override x.Signal     = lazy v.Signal
    override x.Summary () = StringResultItem (v |> ResultValue.map string) :> ResultItem |> ResultItemSource 
    override x.Properties =
        let z = v.ToContainer ()
        let s = v.Signal
        [ BasicResultProperty.FromInt ("count", z, TimingStats.count, s, TimingStatsCountId);
          BasicResultProperty.FromDouble ("mean", z, TimingStats.mean, s, TimingStatsMeanId);
          BasicResultProperty.FromDouble ("std", z, TimingStats.deviation, s, TimingStatsDeviationId);
          BasicResultProperty.FromDouble ("var", z, TimingStats.variance, s, TimingStatsVarianceId);
          BasicResultProperty.FromDouble ("min", z, TimingStats.minimum, s, TimingStatsMinimumId);
          BasicResultProperty.FromDouble ("max", z, TimingStats.maximum, s, TimingStatsMaximumId);
          BasicResultProperty.FromDouble ("minTime", z, TimingStats.minimumTime, s, TimingStatsMinimumTimeId);
          BasicResultProperty.FromDouble ("maxTime", z, TimingStats.maximumTime, s, TimingStatsMaximumTimeId);
          BasicResultProperty.FromDouble ("startTime", z, TimingStats.startTime, s, TimingStatsStartTimeId);
          BasicResultProperty.FromDouble ("lastTime", z, TimingStats.lastTime, s, TimingStatsLastTimeId);
          BasicResultProperty.FromDouble ("sum", z, TimingStats.sum, s, TimingStatsSumId);
          BasicResultProperty.FromDouble ("sum2", z, TimingStats.sum2, s, TimingStatsSum2Id) ]

and BasicResultArray (name, items, subscript, id) =

    inherit ResultArray ()

    let signal = 
        lazy (items 
                |> Array.map ResultSource.signal
                |> Array.fold ResultSignal.merge EmptyResultSignal)

    let itemType = if items.Length > 0 then ResultSource.typeId items.[0] else EmptyId

    override x.Name       = name
    override x.Id         = id
    override x.TypeId     = ArrayId itemType
    override x.Items      = items
    override x.Subscript  = subscript
    override x.Signal     = signal
    override x.Summary () =

        let items' = items |> Array.map ResultSource.summary
        BasicResultArray (name, items', subscript, id) :> ResultArray |> ResultArraySource

type QueueResultObject<'a> (c: ResultContainer<Queue<'a>>) =

    inherit ResultObject ()

    override x.Name       = c.Name
    override x.Id         = c.Id
    override x.TypeId     = FiniteQueueId
    override x.Signal     = lazy c.Signal
    override x.Summary () = QueueResultSummary<'a> (c) :> ResultObject |> ResultObjectSource 
    override x.Properties =
        [ BasicResultProperty.FromAnyConst ("inputStrategy", c, Queue.inputStrategy, QueueInputStrategyId);
          BasicResultProperty.FromAnyConst ("storingStrategy", c, Queue.storingStrategy, QueueStoringStrategyId);
          BasicResultProperty.FromAnyConst ("outputStrategy", c, Queue.outputStrategy, QueueOutputStrategyId);
          BasicResultProperty.FromBoolean ("isEmpty", c, Queue.isEmpty, Queue.countChanged_, QueueEmptyId);
          BasicResultProperty.FromBoolean ("isFull", c, Queue.isFull, Queue.countChanged_, QueueFullId);
          BasicResultProperty.FromIntConst ("maxCount", c, Queue.maxCount, QueueMaxCountId);
          BasicResultProperty.FromInt ("count", c, Queue.count, Queue.countChanged_, QueueCountId);
          BasicResultProperty.FromIntTimingStats ("countStats", c, Queue.countStats, Queue.countChanged_, QueueCountStatsId);
          BasicResultProperty.FromInt ("inputCount", c, Queue.inputCount, Queue.inputCountChanged_, QueueInputCountId);
          BasicResultProperty.FromInt ("lostCount", c, Queue.lostCount, Queue.lostCountChanged_, QueueLostCountId);
          BasicResultProperty.FromInt ("storeCount", c, Queue.storeCount, Queue.storeCountChanged_, QueueStoreCountId);
          BasicResultProperty.FromInt ("outputRequestCount", c, Queue.outputRequestCount, Queue.outputRequestCountChanged_, QueueOutputRequestCountId);
          BasicResultProperty.FromInt ("outputCount", c, Queue.outputCount, Queue.outputCountChanged_, QueueOutputCountId);
          BasicResultProperty.FromDouble ("loadFactor", c, Queue.loadFactor, Queue.loadFactorChanged_, QueueLoadFactorId);
          BasicResultProperty.FromDouble ("inputRate", c, Queue.inputRate, QueueInputRateId);
          BasicResultProperty.FromDouble ("storeRate", c, Queue.storeRate, QueueStoreRateId);
          BasicResultProperty.FromDouble ("outputRequetsRate", c, Queue.outputRequestRate, QueueOutputRequestRateId);
          BasicResultProperty.FromDouble ("outputRate", c, Queue.outputRate, QueueOutputRateId);
          BasicResultProperty.FromDoubleStats ("waitTime", c, Queue.waitTime, Queue.waitTimeChanged_, QueueWaitTimeId);
          BasicResultProperty.FromDoubleStats ("totalWaitTime", c, Queue.totalWaitTime, Queue.totalWaitTimeChanged_, QueueTotalWaitTimeId);
          BasicResultProperty.FromDoubleStats ("inputWaitTime", c, Queue.inputWaitTime, Queue.inputWaitTimeChanged_, QueueInputWaitTimeId);
          BasicResultProperty.FromDoubleStats ("outputWaitTime", c, Queue.outputWaitTime, Queue.outputWaitTimeChanged_, QueueOutputWaitTimeId);
          BasicResultProperty.FromDouble ("rate", c, Queue.rate, Queue.rateChanged_, QueueRateId)]

and QueueResultSummary<'a> (c: ResultContainer<Queue<'a>>) =

    inherit ResultObject ()

    override x.Name       = c.Name
    override x.Id         = c.Id
    override x.TypeId     = FiniteQueueId
    override x.Signal     = lazy c.Signal
    override x.Summary () = QueueResultSummary<'a> (c) :> ResultObject |> ResultObjectSource 
    override x.Properties =
        [ BasicResultProperty.FromIntConst ("maxCount", c, Queue.maxCount, QueueMaxCountId);
          BasicResultProperty.FromIntTimingStats ("countStats", c, Queue.countStats, Queue.countChanged_, QueueCountStatsId);
          BasicResultProperty.FromInt ("inputCount", c, Queue.inputCount, Queue.inputCountChanged_, QueueInputCountId);
          BasicResultProperty.FromInt ("lostCount", c, Queue.lostCount, Queue.lostCountChanged_, QueueLostCountId);
          BasicResultProperty.FromInt ("storeCount", c, Queue.storeCount, Queue.storeCountChanged_, QueueStoreCountId);
          BasicResultProperty.FromInt ("outputRequestCount", c, Queue.outputRequestCount, Queue.outputRequestCountChanged_, QueueOutputRequestCountId);
          BasicResultProperty.FromInt ("outputCount", c, Queue.outputCount, Queue.outputCountChanged_, QueueOutputCountId);
          BasicResultProperty.FromDouble ("loadFactor", c, Queue.loadFactor, Queue.loadFactorChanged_, QueueLoadFactorId);
          BasicResultProperty.FromDoubleStats ("waitTime", c, Queue.waitTime, Queue.waitTimeChanged_, QueueWaitTimeId);
          BasicResultProperty.FromDouble ("rate", c, Queue.rate, Queue.rateChanged_, QueueRateId)]

type InfiniteQueueResultObject<'a> (c: ResultContainer<InfiniteQueue<'a>>) =

    inherit ResultObject ()

    override x.Name       = c.Name
    override x.Id         = c.Id
    override x.TypeId     = InfiniteQueueId
    override x.Signal     = lazy c.Signal
    override x.Summary () = InfiniteQueueResultSummary<'a> (c) :> ResultObject |> ResultObjectSource 
    override x.Properties =
        [ BasicResultProperty.FromAnyConst ("storingStrategy", c, InfiniteQueue.storingStrategy, QueueStoringStrategyId);
          BasicResultProperty.FromAnyConst ("outputStrategy", c, InfiniteQueue.outputStrategy, QueueOutputStrategyId);
          BasicResultProperty.FromBoolean ("isEmpty", c, InfiniteQueue.isEmpty, InfiniteQueue.countChanged_, QueueEmptyId);
          BasicResultProperty.FromInt ("count", c, InfiniteQueue.count, InfiniteQueue.countChanged_, QueueCountId);
          BasicResultProperty.FromIntTimingStats ("countStats", c, InfiniteQueue.countStats, InfiniteQueue.countChanged_, QueueCountStatsId);
          BasicResultProperty.FromInt ("storeCount", c, InfiniteQueue.storeCount, InfiniteQueue.storeCountChanged_, QueueStoreCountId);
          BasicResultProperty.FromInt ("outputRequestCount", c, InfiniteQueue.outputRequestCount, InfiniteQueue.outputRequestCountChanged_, QueueOutputRequestCountId);
          BasicResultProperty.FromInt ("outputCount", c, InfiniteQueue.outputCount, InfiniteQueue.outputCountChanged_, QueueOutputCountId);
          BasicResultProperty.FromDouble ("storeRate", c, InfiniteQueue.storeRate, QueueStoreRateId);
          BasicResultProperty.FromDouble ("outputRequetsRate", c, InfiniteQueue.outputRequestRate, QueueOutputRequestRateId);
          BasicResultProperty.FromDouble ("outputRate", c, InfiniteQueue.outputRate, QueueOutputRateId);
          BasicResultProperty.FromDoubleStats ("waitTime", c, InfiniteQueue.waitTime, InfiniteQueue.waitTimeChanged_, QueueWaitTimeId);
          BasicResultProperty.FromDoubleStats ("outputWaitTime", c, InfiniteQueue.outputWaitTime, InfiniteQueue.outputWaitTimeChanged_, QueueOutputWaitTimeId);
          BasicResultProperty.FromDouble ("rate", c, InfiniteQueue.rate, InfiniteQueue.rateChanged_, QueueRateId)]

and InfiniteQueueResultSummary<'a> (c: ResultContainer<InfiniteQueue<'a>>) =

    inherit ResultObject ()

    override x.Name       = c.Name
    override x.Id         = c.Id
    override x.TypeId     = InfiniteQueueId
    override x.Signal     = lazy c.Signal
    override x.Summary () = InfiniteQueueResultSummary<'a> (c) :> ResultObject |> ResultObjectSource 
    override x.Properties =
        [ BasicResultProperty.FromIntTimingStats ("countStats", c, InfiniteQueue.countStats, InfiniteQueue.countChanged_, QueueCountStatsId);
          BasicResultProperty.FromInt ("storeCount", c, InfiniteQueue.storeCount, InfiniteQueue.storeCountChanged_, QueueStoreCountId);
          BasicResultProperty.FromInt ("outputRequestCount", c, InfiniteQueue.outputRequestCount, InfiniteQueue.outputRequestCountChanged_, QueueOutputRequestCountId);
          BasicResultProperty.FromInt ("outputCount", c, InfiniteQueue.outputCount, InfiniteQueue.outputCountChanged_, QueueOutputCountId);
          BasicResultProperty.FromDoubleStats ("waitTime", c, InfiniteQueue.waitTime, InfiniteQueue.waitTimeChanged_, QueueWaitTimeId);
          BasicResultProperty.FromDouble ("rate", c, InfiniteQueue.rate, InfiniteQueue.rateChanged_, QueueRateId)]

type ServerResultObject<'s, 'a, 'b> (c: ResultContainer<Server<'s, 'a, 'b>>) =

    inherit ResultObject ()

    override x.Name       = c.Name
    override x.Id         = c.Id
    override x.TypeId     = ServerId
    override x.Signal     = lazy c.Signal
    override x.Summary () = ServerResultSummary<'s, 'a, 'b> (c) :> ResultObject |> ResultObjectSource 
    override x.Properties =
        [ BasicResultProperty.FromAnyConst ("initState", c, Server.initState, ServerInitStateId);
          BasicResultProperty.FromAny ("state", c, Server.state, Server.stateChanged_, ServerStateId);
          BasicResultProperty.FromDouble ("totalInputWaitTime", c, Server.totalInputWaitTime, Server.totalInputWaitTimeChanged_, ServerTotalInputWaitTimeId);
          BasicResultProperty.FromDouble ("totalProcessingTime", c, Server.totalProcessingTime, Server.totalProcessingTimeChanged_, ServerTotalProcessingTimeId);
          BasicResultProperty.FromDouble ("totalOutputWaitTime", c, Server.totalOutputWaitTime, Server.totalOutputWaitTimeChanged_, ServerTotalOutputWaitTimeId);
          BasicResultProperty.FromDouble ("totalPreemptionTime", c, Server.totalPreemptionTime, Server.totalPreemptionTimeChanged_, ServerTotalPreemptionTimeId);
          BasicResultProperty.FromDoubleStats ("inputWaitTime", c, Server.inputWaitTime, Server.inputWaitTimeChanged_, ServerInputWaitTimeId);
          BasicResultProperty.FromDoubleStats ("processingTime", c, Server.processingTime, Server.processingTimeChanged_, ServerProcessingTimeId);
          BasicResultProperty.FromDoubleStats ("outputWaitTime", c, Server.outputWaitTime, Server.outputWaitTimeChanged_, ServerOutputWaitTimeId);
          BasicResultProperty.FromDoubleStats ("preemptionTime", c, Server.preemptionTime, Server.preemptionTimeChanged_, ServerPreemptionTimeId);
          BasicResultProperty.FromDouble ("inputWaitFactor", c, Server.inputWaitFactor, Server.inputWaitFactorChanged_, ServerInputWaitFactorId);
          BasicResultProperty.FromDouble ("processingFactor", c, Server.processingFactor, Server.processingFactorChanged_, ServerProcessingFactorId);
          BasicResultProperty.FromDouble ("outputWaitFactor", c, Server.outputWaitFactor, Server.outputWaitFactorChanged_, ServerOutputWaitFactorId);
          BasicResultProperty.FromDouble ("preemptionFactor", c, Server.preemptionFactor, Server.preemptionFactorChanged_, ServerPreemptionFactorId) ]

and ServerResultSummary<'s, 'a, 'b> (c: ResultContainer<Server<'s, 'a, 'b>>) =

    inherit ResultObject ()

    override x.Name       = c.Name
    override x.Id         = c.Id
    override x.TypeId     = ServerId
    override x.Signal     = lazy c.Signal
    override x.Summary () = ServerResultSummary<'s, 'a, 'b> (c) :> ResultObject |> ResultObjectSource 
    override x.Properties =
        [ BasicResultProperty.FromDoubleStats ("inputWaitTime", c, Server.inputWaitTime, Server.inputWaitTimeChanged_, ServerInputWaitTimeId);
          BasicResultProperty.FromDoubleStats ("processingTime", c, Server.processingTime, Server.processingTimeChanged_, ServerProcessingTimeId);
          BasicResultProperty.FromDoubleStats ("outputWaitTime", c, Server.outputWaitTime, Server.outputWaitTimeChanged_, ServerOutputWaitTimeId);
          BasicResultProperty.FromDoubleStats ("preemptionTime", c, Server.preemptionTime, Server.preemptionTimeChanged_, ServerPreemptionTimeId);
          BasicResultProperty.FromDouble ("inputWaitFactor", c, Server.inputWaitFactor, Server.inputWaitFactorChanged_, ServerInputWaitFactorId);
          BasicResultProperty.FromDouble ("processingFactor", c, Server.processingFactor, Server.processingFactorChanged_, ServerProcessingFactorId);
          BasicResultProperty.FromDouble ("outputWaitFactor", c, Server.outputWaitFactor, Server.outputWaitFactorChanged_, ServerOutputWaitFactorId);
          BasicResultProperty.FromDouble ("preemptionFactor", c, Server.preemptionFactor, Server.preemptionFactorChanged_, ServerPreemptionFactorId) ]

type ArrivalTimerResultObject (c: ResultContainer<ArrivalTimer>) =

    inherit ResultObject ()

    override x.Name       = c.Name
    override x.Id         = c.Id
    override x.TypeId     = ArrivalTimerId
    override x.Signal     = lazy c.Signal
    override x.Summary () = ArrivalTimerResultSummary (c) :> ResultObject |> ResultObjectSource 
    override x.Properties =
        [ BasicResultProperty.FromDoubleStats ("processingTime", c, ArrivalTimer.processingTime, ArrivalTimer.processingTimeChanged_, ArrivalProcessingTimeId) ]

and ArrivalTimerResultSummary (c: ResultContainer<ArrivalTimer>) =

    inherit ResultObject ()

    override x.Name       = c.Name
    override x.Id         = c.Id
    override x.TypeId     = ArrivalTimerId
    override x.Signal     = lazy c.Signal
    override x.Summary () = ArrivalTimerResultSummary (c) :> ResultObject |> ResultObjectSource 
    override x.Properties =
        [ BasicResultProperty.FromDoubleStats ("processingTime", c, ArrivalTimer.processingTime, ArrivalTimer.processingTimeChanged_, ArrivalProcessingTimeId) ]

type ResourceResultObject (c: ResultContainer<Resource>) =

    inherit ResultObject ()

    override x.Name       = c.Name
    override x.Id         = c.Id
    override x.TypeId     = ResourceId
    override x.Signal     = lazy c.Signal
    override x.Summary () = ResourceResultSummary (c) :> ResultObject |> ResultObjectSource 
    override x.Properties =
        [ BasicResultProperty.FromInt ("queueCount", c, Resource.queueCount, Resource.queueCountChanged_, ResourceQueueCountId);
          BasicResultProperty.FromIntTimingStats ("queueCountStats", c, Resource.queueCountStats, Resource.queueCountChanged_, ResourceQueueCountStatsId);
          BasicResultProperty.FromDouble ("totalWaitTime", c, Resource.totalWaitTime, Resource.waitTimeChanged_, ResourceTotalWaitTimeId);
          BasicResultProperty.FromDoubleStats ("waitTime", c, Resource.waitTime, Resource.waitTimeChanged_, ResourceWaitTimeId);
          BasicResultProperty.FromInt ("count", c, Resource.count, Resource.countChanged_, ResourceCountId);
          BasicResultProperty.FromIntTimingStats ("countStats", c, Resource.countStats, Resource.countChanged_, ResourceCountStatsId);
          BasicResultProperty.FromInt ("utilisationCount", c, Resource.utilisationCount, Resource.utilisationCountChanged_, ResourceUtilisationCountId);
          BasicResultProperty.FromIntTimingStats ("utilisationCountStats", c, Resource.utilisationCountStats, Resource.utilisationCountChanged_, ResourceUtilisationCountStatsId) ]

and ResourceResultSummary (c: ResultContainer<Resource>) =

    inherit ResultObject ()

    override x.Name       = c.Name
    override x.Id         = c.Id
    override x.TypeId     = ResourceId
    override x.Signal     = lazy c.Signal
    override x.Summary () = ResourceResultSummary (c) :> ResultObject |> ResultObjectSource 
    override x.Properties =
        [ BasicResultProperty.FromIntTimingStats ("queueCountStats", c, Resource.queueCountStats, Resource.queueCountChanged_, ResourceQueueCountStatsId);
          BasicResultProperty.FromDoubleStats ("waitTime", c, Resource.waitTime, Resource.waitTimeChanged_, ResourceWaitTimeId);
          BasicResultProperty.FromIntTimingStats ("countStats", c, Resource.countStats, Resource.countChanged_, ResourceCountStatsId);
          BasicResultProperty.FromIntTimingStats ("utilisationCountStats", c, Resource.utilisationCountStats, Resource.utilisationCountChanged_, ResourceUtilisationCountStatsId) ]

type PreemptibleResourceResultObject (c: ResultContainer<PreemptibleResource>) =

    inherit ResultObject ()

    override x.Name       = c.Name
    override x.Id         = c.Id
    override x.TypeId     = ResourceId
    override x.Signal     = lazy c.Signal
    override x.Summary () = PreemptibleResourceResultSummary (c) :> ResultObject |> ResultObjectSource 
    override x.Properties =
        [ BasicResultProperty.FromInt ("queueCount", c, PreemptibleResource.queueCount, PreemptibleResource.queueCountChanged_, ResourceQueueCountId);
          BasicResultProperty.FromIntTimingStats ("queueCountStats", c, PreemptibleResource.queueCountStats, PreemptibleResource.queueCountChanged_, ResourceQueueCountStatsId);
          BasicResultProperty.FromDouble ("totalWaitTime", c, PreemptibleResource.totalWaitTime, PreemptibleResource.waitTimeChanged_, ResourceTotalWaitTimeId);
          BasicResultProperty.FromDoubleStats ("waitTime", c, PreemptibleResource.waitTime, PreemptibleResource.waitTimeChanged_, ResourceWaitTimeId);
          BasicResultProperty.FromInt ("count", c, PreemptibleResource.count, PreemptibleResource.countChanged_, ResourceCountId);
          BasicResultProperty.FromIntTimingStats ("countStats", c, PreemptibleResource.countStats, PreemptibleResource.countChanged_, ResourceCountStatsId);
          BasicResultProperty.FromInt ("utilisationCount", c, PreemptibleResource.utilisationCount, PreemptibleResource.utilisationCountChanged_, ResourceUtilisationCountId);
          BasicResultProperty.FromIntTimingStats ("utilisationCountStats", c, PreemptibleResource.utilisationCountStats, PreemptibleResource.utilisationCountChanged_, ResourceUtilisationCountStatsId) ]

and PreemptibleResourceResultSummary (c: ResultContainer<PreemptibleResource>) =

    inherit ResultObject ()

    override x.Name       = c.Name
    override x.Id         = c.Id
    override x.TypeId     = ResourceId
    override x.Signal     = lazy c.Signal
    override x.Summary () = PreemptibleResourceResultSummary (c) :> ResultObject |> ResultObjectSource 
    override x.Properties =
        [ BasicResultProperty.FromIntTimingStats ("queueCountStats", c, PreemptibleResource.queueCountStats, PreemptibleResource.queueCountChanged_, ResourceQueueCountStatsId);
          BasicResultProperty.FromDoubleStats ("waitTime", c, PreemptibleResource.waitTime, PreemptibleResource.waitTimeChanged_, ResourceWaitTimeId);
          BasicResultProperty.FromIntTimingStats ("countStats", c, PreemptibleResource.countStats, PreemptibleResource.countChanged_, ResourceCountStatsId);
          BasicResultProperty.FromIntTimingStats ("utilisationCountStats", c, PreemptibleResource.utilisationCountStats, PreemptibleResource.utilisationCountChanged_, ResourceUtilisationCountStatsId) ]

[<AutoOpen>]
module ResultPropertyExtensions =

    type ResultProperty with

        static member FromIntConst (label: ResultName, comp: ResultContainer<'a>, f: 'a -> int, id: ResultId): ResultProperty =
            BasicResultProperty.FromIntConst (label, comp, f, id)

        static member FromInt (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<int>, id: ResultId): ResultProperty =
            BasicResultProperty.FromInt (label, comp, f, id)

        static member FromInt (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<int>, g: 'a -> Signal<unit>, id: ResultId): ResultProperty =
            BasicResultProperty.FromInt (label, comp, f, g, id)

        static member FromInt (label: ResultName, comp: ResultContainer<ResultData<'a>>, f: 'a -> int, signal: ResultSignal, id: ResultId): ResultProperty =
            BasicResultProperty.FromInt (label, comp, f, signal, id)

        static member FromIntListConst (label: ResultName, comp: ResultContainer<'a>, f: 'a -> int list, id: ResultId): ResultProperty =
            BasicResultProperty.FromIntListConst (label, comp, f, id)

        static member FromIntList (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<int list>, id: ResultId): ResultProperty =
            BasicResultProperty.FromIntList (label, comp, f, id)

        static member FromIntList (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<int list>, g: 'a -> Signal<unit>, id: ResultId): ResultProperty =
            BasicResultProperty.FromIntList (label, comp, f, g, id)

        static member FromIntList (label: ResultName, comp: ResultContainer<ResultData<'a>>, f: 'a -> int list, signal: ResultSignal, id: ResultId): ResultProperty =
            BasicResultProperty.FromIntList (label, comp, f, signal, id)

        static member FromIntStatsConst (label: ResultName, comp: ResultContainer<'a>, f: 'a -> SamplingStats<int>, id: ResultId): ResultProperty =
            BasicResultProperty.FromIntStatsConst (label, comp, f, id)

        static member FromIntStats (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<SamplingStats<int>>, id: ResultId): ResultProperty =
            BasicResultProperty.FromIntStats (label, comp, f, id)

        static member FromIntStats (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<SamplingStats<int>>, g: 'a -> Signal<unit>, id: ResultId): ResultProperty =
            BasicResultProperty.FromIntStats (label, comp, f, g, id)

        static member FromIntStats (label: ResultName, comp: ResultContainer<ResultData<'a>>, f: 'a -> SamplingStats<int>, signal: ResultSignal, id: ResultId): ResultProperty =
            BasicResultProperty.FromIntStats (label, comp, f, signal, id)

        static member FromIntTimingStatsConst (label: ResultName, comp: ResultContainer<'a>, f: 'a -> TimingStats<int>, id: ResultId): ResultProperty =
            BasicResultProperty.FromIntTimingStatsConst (label, comp, f, id)

        static member FromIntTimingStats (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<TimingStats<int>>, id: ResultId): ResultProperty =
            BasicResultProperty.FromIntTimingStats (label, comp, f, id)

        static member FromIntTimingStats (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<TimingStats<int>>, g: 'a -> Signal<unit>, id: ResultId): ResultProperty =
            BasicResultProperty.FromIntTimingStats (label, comp, f, g, id)

        static member FromIntTimingStats (label: ResultName, comp: ResultContainer<ResultData<'a>>, f: 'a -> TimingStats<int>, signal: ResultSignal, id: ResultId): ResultProperty =
            BasicResultProperty.FromIntTimingStats (label, comp, f, signal, id)

        static member FromDoubleConst (label: ResultName, comp: ResultContainer<'a>, f: 'a -> float, id: ResultId): ResultProperty =
            BasicResultProperty.FromDoubleConst (label, comp, f, id)

        static member FromDouble (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<float>, id: ResultId): ResultProperty =
            BasicResultProperty.FromDouble (label, comp, f, id)

        static member FromDouble (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<float>, g: 'a -> Signal<unit>, id: ResultId): ResultProperty =
            BasicResultProperty.FromDouble (label, comp, f, g, id)

        static member FromDouble (label: ResultName, comp: ResultContainer<ResultData<'a>>, f: 'a -> float, signal: ResultSignal, id: ResultId): ResultProperty =
            BasicResultProperty.FromDouble (label, comp, f, signal, id)

        static member FromDoubleListConst (label: ResultName, comp: ResultContainer<'a>, f: 'a -> float list, id: ResultId): ResultProperty =
            BasicResultProperty.FromDoubleListConst (label, comp, f, id)

        static member FromDoubleList (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<float list>, id: ResultId): ResultProperty =
            BasicResultProperty.FromDoubleList (label, comp, f, id)

        static member FromDoubleList (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<float list>, g: 'a -> Signal<unit>, id: ResultId): ResultProperty =
            BasicResultProperty.FromDoubleList (label, comp, f, g, id)

        static member FromDoubleList (label: ResultName, comp: ResultContainer<ResultData<'a>>, f: 'a -> float list, signal: ResultSignal, id: ResultId): ResultProperty =
            BasicResultProperty.FromDoubleList (label, comp, f, signal, id)

        static member FromDoubleStatsConst (label: ResultName, comp: ResultContainer<'a>, f: 'a -> SamplingStats<float>, id: ResultId): ResultProperty =
            BasicResultProperty.FromDoubleStatsConst (label, comp, f, id)

        static member FromDoubleStats (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<SamplingStats<float>>, id: ResultId): ResultProperty =
            BasicResultProperty.FromDoubleStats (label, comp, f, id)

        static member FromDoubleStats (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<SamplingStats<float>>, g: 'a -> Signal<unit>, id: ResultId): ResultProperty =
            BasicResultProperty.FromDoubleStats (label, comp, f, g, id)

        static member FromDoubleStats (label: ResultName, comp: ResultContainer<ResultData<'a>>, f: 'a -> SamplingStats<float>, signal: ResultSignal, id: ResultId): ResultProperty =
            BasicResultProperty.FromDoubleStats (label, comp, f, signal, id)

        static member FromDoubleTimingStatsConst (label: ResultName, comp: ResultContainer<'a>, f: 'a -> TimingStats<float>, id: ResultId): ResultProperty =
            BasicResultProperty.FromDoubleTimingStatsConst (label, comp, f, id)

        static member FromDoubleTimingStats (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<TimingStats<float>>, id: ResultId): ResultProperty =
            BasicResultProperty.FromDoubleTimingStats (label, comp, f, id)

        static member FromDoubleTimingStats (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<TimingStats<float>>, g: 'a -> Signal<unit>, id: ResultId): ResultProperty =
            BasicResultProperty.FromDoubleTimingStats (label, comp, f, g, id)

        static member FromDoubleTimingStats (label: ResultName, comp: ResultContainer<ResultData<'a>>, f: 'a -> TimingStats<float>, signal: ResultSignal, id: ResultId): ResultProperty =
            BasicResultProperty.FromDoubleTimingStats (label, comp, f, signal, id)

        static member FromStringConst (label: ResultName, comp: ResultContainer<'a>, f: 'a -> string, id: ResultId): ResultProperty =
            BasicResultProperty.FromStringConst (label, comp, f, id)

        static member FromString (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<string>, id: ResultId): ResultProperty =
            BasicResultProperty.FromString (label, comp, f, id)

        static member FromString (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<string>, g: 'a -> Signal<unit>, id: ResultId): ResultProperty =
            BasicResultProperty.FromString (label, comp, f, g, id)

        static member FromString (label: ResultName, comp: ResultContainer<ResultData<'a>>, f: 'a -> string, signal: ResultSignal, id: ResultId): ResultProperty =
            BasicResultProperty.FromString (label, comp, f, signal, id)

        static member FromBooleanConst (label: ResultName, comp: ResultContainer<'a>, f: 'a -> bool, id: ResultId): ResultProperty =
            BasicResultProperty.FromBooleanConst (label, comp, f, id)

        static member FromBoolean (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<bool>, id: ResultId): ResultProperty =
            BasicResultProperty.FromBoolean (label, comp, f, id)

        static member FromBoolean (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<bool>, g: 'a -> Signal<unit>, id: ResultId): ResultProperty =
            BasicResultProperty.FromBoolean (label, comp, f, g, id)

        static member FromBoolean (label: ResultName, comp: ResultContainer<ResultData<'a>>, f: 'a -> bool, signal: ResultSignal, id: ResultId): ResultProperty =
            BasicResultProperty.FromBoolean (label, comp, f, signal, id)

        static member FromAnyConst (label: ResultName, comp: ResultContainer<'a>, f: 'a -> 'b, id: ResultId): ResultProperty =
            BasicResultProperty.FromAnyConst (label, comp, f, id)

        static member FromAny (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<'b>, id: ResultId): ResultProperty =
            BasicResultProperty.FromAny (label, comp, f, id)

        static member FromAny (label: ResultName, comp: ResultContainer<'a>, f: 'a -> Eventive<'b>, g: 'a -> Signal<unit>, id: ResultId): ResultProperty =
            BasicResultProperty.FromAny (label, comp, f, g, id)

        static member FromAny (label: ResultName, comp: ResultContainer<ResultData<'a>>, f: 'a -> 'b, signal: ResultSignal, id: ResultId): ResultProperty =
            BasicResultProperty.FromAny (label, comp, f, signal, id)
    
[<AutoOpen>]
module ResultSourceExtensions =

    type ResultSource with

        member x.Signal = ResultSource.signal x
        member x.Summary () = ResultSource.summary x

        static member From (name: ResultName, items: ResultSource array, subscript: ResultName array, id:ResultId) =
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, items: ResultSource array, subscript: ResultName array, ?descr: ResultDescription) =
            ResultSource.From (name, items, subscript, CustomResultId descr)

        static member From (name: ResultName, x: Parameter<_>, id: ResultId) =
            match box x with
            | :? Parameter<int> as z ->
                IntResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Parameter<int list> as z ->
                IntListResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Parameter<SamplingStats<int>> as z ->
                IntStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Parameter<TimingStats<int>> as z ->
                IntTimingStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Parameter<float> as z ->
                DoubleResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Parameter<float list> as z ->
                DoubleListResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Parameter<SamplingStats<float>> as z ->
                DoubleStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Parameter<TimingStats<float>> as z ->
                DoubleTimingStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Parameter<string> as z ->
                StringResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | _ ->
                let z = Parameter.map (fun a -> a.ToString()) x
                StringResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource

        static member From (name: ResultName, x: Parameter<_>, ?descr: ResultDescription) =
            ResultSource.From (name, x, CustomResultId descr)

        static member From (name: ResultName, x: Simulation<_>, id: ResultId) =
            match box x with
            | :? Simulation<int> as z ->
                IntResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Simulation<int list> as z ->
                IntListResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Simulation<SamplingStats<int>> as z ->
                IntStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Simulation<TimingStats<int>> as z ->
                IntTimingStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Simulation<float> as z ->
                DoubleResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Simulation<float list> as z ->
                DoubleListResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Simulation<SamplingStats<float>> as z ->
                DoubleStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Simulation<TimingStats<float>> as z ->
                DoubleTimingStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Simulation<string> as z ->
                StringResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | _ ->
                let z = Simulation.map (fun a -> a.ToString()) x
                StringResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource

        static member From (name: ResultName, x: Simulation<_>, ?descr: ResultDescription) =
            ResultSource.From (name, x, CustomResultId descr)

        static member From (name: ResultName, x: Dynamics<_>, id: ResultId) =
            match box x with
            | :? Dynamics<int> as z ->
                IntResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Dynamics<int list> as z ->
                IntListResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Dynamics<SamplingStats<int>> as z ->
                IntStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Dynamics<TimingStats<int>> as z ->
                IntTimingStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Dynamics<float> as z ->
                DoubleResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Dynamics<float list> as z ->
                DoubleListResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Dynamics<SamplingStats<float>> as z ->
                DoubleStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Dynamics<TimingStats<float>> as z ->
                DoubleTimingStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Dynamics<string> as z ->
                StringResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | _ ->
                let z = Dynamics.map (fun a -> a.ToString()) x
                StringResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource

        static member From (name: ResultName, x: Dynamics<_>, ?descr: ResultDescription) =
            ResultSource.From (name, x, CustomResultId descr)

        static member From (name: ResultName, x: Eventive<_>, id: ResultId) =
            match box x with
            | :? Eventive<int> as z ->
                IntResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Eventive<int list> as z ->
                IntListResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Eventive<SamplingStats<int>> as z ->
                IntStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Eventive<TimingStats<int>> as z ->
                IntTimingStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Eventive<float> as z ->
                DoubleResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Eventive<float list> as z ->
                DoubleListResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Eventive<SamplingStats<float>> as z ->
                DoubleStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Eventive<TimingStats<float>> as z ->
                DoubleTimingStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Eventive<string> as z ->
                StringResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | _ ->
                let z = Eventive.map (fun a -> a.ToString()) x
                StringResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource

        static member From (name: ResultName, x: Eventive<_>, ?descr: ResultDescription) =
            ResultSource.From (name, x, CustomResultId descr)

        static member From (name: ResultName, x: ISignalable<_>, id: ResultId) =
            match box x with
            | :? ISignalable<int> as z ->
                IntResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? ISignalable<int list> as z ->
                IntListResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? ISignalable<SamplingStats<int>> as z ->
                IntStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? ISignalable<TimingStats<int>> as z ->
                IntTimingStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? ISignalable<float> as z ->
                DoubleResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? ISignalable<float list> as z ->
                DoubleListResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? ISignalable<SamplingStats<float>> as z ->
                DoubleStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? ISignalable<TimingStats<float>> as z ->
                DoubleTimingStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? ISignalable<string> as z ->
                StringResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | _ ->
                let z = Eventive.map (fun a -> a.ToString()) (Signalable.read x)
                StringResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource

        static member From (name: ResultName, x: ISignalable<_>, ?descr: ResultDescription) =
            ResultSource.From (name, x, CustomResultId descr)

        static member From (name: ResultName, x: Ref<_>, id: ResultId) =
            match box x with
            | :? Ref<int> as z ->
                IntResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Ref<int list> as z ->
                IntListResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Ref<SamplingStats<int>> as z ->
                IntStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Ref<TimingStats<int>> as z ->
                IntTimingStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Ref<float> as z ->
                DoubleResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Ref<float list> as z ->
                DoubleListResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Ref<SamplingStats<float>> as z ->
                DoubleStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Ref<TimingStats<float>> as z ->
                DoubleTimingStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Ref<string> as z ->
                StringResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | _ ->
                let z = Eventive.map (fun a -> a.ToString()) (Ref.read x)
                StringResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource

        static member From (name: ResultName, x: Ref<_>, ?descr: ResultDescription) =
            ResultSource.From (name, x, CustomResultId descr)

        static member From (name: ResultName, x: _ ref, id: ResultId) =
            ResultSource.From (name, eventive { return !x }, id)

        static member From (name: ResultName, x: _ ref, ?descr: ResultDescription) =
            ResultSource.From (name, x, CustomResultId descr)

        static member From (name: ResultName, x: Var<_>, id: ResultId) =
            match box x with
            | :? Var<int> as z ->
                IntResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Var<int list> as z ->
                IntListResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Var<SamplingStats<int>> as z ->
                IntStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Var<TimingStats<int>> as z ->
                IntTimingStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Var<float> as z ->
                DoubleResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Var<float list> as z ->
                DoubleListResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Var<SamplingStats<float>> as z ->
                DoubleStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Var<TimingStats<float>> as z ->
                DoubleTimingStatsResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | :? Var<string> as z ->
                StringResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource
            | _ ->
                let z = Eventive.map (fun a -> a.ToString()) (Var.read x)
                StringResultItem (z.ToResultValue (name, id)) :> ResultItem |> ResultItemSource

        static member From (name: ResultName, x: Var<_>, ?descr: ResultDescription) =
            ResultSource.From (name, x, CustomResultId descr)

        static member From (name: ResultName, x: Queue<_>, id: ResultId) =
            let c = { new ResultContainer<_> () with
                        override c.Name   = name
                        override c.Id     = id
                        override c.Data   = x
                        override c.Signal = Queue.changed_ x |> ResultSignal }
            QueueResultObject<_> (c) :> ResultObject |> ResultObjectSource

        static member From (name: ResultName, x: Queue<_>, ?descr: ResultDescription) =
            ResultSource.From (name, x, CustomResultId descr)

        static member From (name: ResultName, x: InfiniteQueue<_>, id: ResultId) =
            let c = { new ResultContainer<_> () with
                        override c.Name   = name
                        override c.Id     = id
                        override c.Data   = x
                        override c.Signal = InfiniteQueue.changed_ x |> ResultSignal }
            InfiniteQueueResultObject<_> (c) :> ResultObject |> ResultObjectSource

        static member From (name: ResultName, x: InfiniteQueue<_>, ?descr: ResultDescription) =
            ResultSource.From (name, x, CustomResultId descr)

        static member From (name: ResultName, x: Server<_, _, _>, id: ResultId) =
            let c = { new ResultContainer<_> () with
                        override c.Name   = name
                        override c.Id     = id
                        override c.Data   = x
                        override c.Signal = Server.changed_ x |> ResultSignal }
            ServerResultObject<_, _, _> (c) :> ResultObject |> ResultObjectSource

        static member From (name: ResultName, x: Server<_, _, _>, ?descr: ResultDescription) =
            ResultSource.From (name, x, CustomResultId descr)

        static member From (name: ResultName, x: ArrivalTimer, id: ResultId) =
            let c = { new ResultContainer<_> () with
                        override c.Name   = name
                        override c.Id     = id
                        override c.Data   = x
                        override c.Signal = ArrivalTimer.processingTimeChanged_ x |> ResultSignal }
            ArrivalTimerResultObject (c) :> ResultObject |> ResultObjectSource

        static member From (name: ResultName, x: ArrivalTimer, ?descr: ResultDescription) =
            ResultSource.From (name, x, CustomResultId descr)

        static member From (name: ResultName, x: Resource, id: ResultId) =
            let c = { new ResultContainer<_> () with
                        override c.Name   = name
                        override c.Id     = id
                        override c.Data   = x
                        override c.Signal = Resource.changed_ x |> ResultSignal }
            ResourceResultObject (c) :> ResultObject |> ResultObjectSource

        static member From (name: ResultName, x: Resource, ?descr: ResultDescription) =
            ResultSource.From (name, x, CustomResultId descr)

        static member From (name: ResultName, x: PreemptibleResource, id: ResultId) =
            let c = { new ResultContainer<_> () with
                        override c.Name   = name
                        override c.Id     = id
                        override c.Data   = x
                        override c.Signal = PreemptibleResource.changed_ x |> ResultSignal }
            PreemptibleResourceResultObject (c) :> ResultObject |> ResultObjectSource

        static member From (name: ResultName, x: PreemptibleResource, ?descr: ResultDescription) =
            ResultSource.From (name, x, CustomResultId descr)

        static member From (name: ResultName, xs: Parameter<_> list, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> List.mapi (fun i x -> ResultSource.From (name + subname i, x, id)) |> List.toArray
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: Parameter<_> list, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: Simulation<_> list, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> List.mapi (fun i x -> ResultSource.From (name + subname i, x, id)) |> List.toArray
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: Simulation<_> list, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: Dynamics<_> list, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> List.mapi (fun i x -> ResultSource.From (name + subname i, x, id)) |> List.toArray
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: Dynamics<_> list, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: Eventive<_> list, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> List.mapi (fun i x -> ResultSource.From (name + subname i, x, id)) |> List.toArray
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: Eventive<_> list, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: ISignalable<_> list, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> List.mapi (fun i x -> ResultSource.From (name + subname i, x, id)) |> List.toArray
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: ISignalable<_> list, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: _ ref list, id: ResultId) =
            let xs = xs |> List.map (fun x -> eventive { return !x })
            ResultSource.From (name, xs, id)

        static member From (name: ResultName, xs: _ ref list, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: Ref<_> list, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> List.mapi (fun i x -> ResultSource.From (name + subname i, x, id)) |> List.toArray
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: Ref<_> list, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: Var<_> list, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> List.mapi (fun i x -> ResultSource.From (name + subname i, x, id)) |> List.toArray
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: Var<_> list, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: Queue<_> list, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> List.mapi (fun i x -> ResultSource.From (name + subname i, x, id)) |> List.toArray
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: Queue<_> list, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: InfiniteQueue<_> list, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> List.mapi (fun i x -> ResultSource.From (name + subname i, x, id)) |> List.toArray
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: InfiniteQueue<_> list, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: Server<_, _, _> list, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> List.mapi (fun i x -> ResultSource.From (name + subname i, x, id)) |> List.toArray
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: Server<_, _, _> list, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: ArrivalTimer list, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> List.mapi (fun i x -> ResultSource.From (name + subname i, x, id)) |> List.toArray
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: ArrivalTimer list, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: Resource list, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> List.mapi (fun i x -> ResultSource.From (name + subname i, x, id)) |> List.toArray
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: Resource list, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: PreemptibleResource list, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> List.mapi (fun i x -> ResultSource.From (name + subname i, x, id)) |> List.toArray
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: PreemptibleResource list, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: Parameter<_> array, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> Array.mapi (fun i x -> ResultSource.From (name + subname i, x, id))
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: Parameter<_> array, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: Simulation<_> array, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> Array.mapi (fun i x -> ResultSource.From (name + subname i, x, id))
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: Simulation<_> array, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: Dynamics<_> array, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> Array.mapi (fun i x -> ResultSource.From (name + subname i, x, id))
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: Dynamics<_> array, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: Eventive<_> array, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> Array.mapi (fun i x -> ResultSource.From (name + subname i, x, id))
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: Eventive<_> array, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: ISignalable<_> array, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> Array.mapi (fun i x -> ResultSource.From (name + subname i, x, id))
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: ISignalable<_> array, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: _ ref array, id: ResultId) =
            let xs = xs |> Array.map (fun x -> eventive { return !x })
            ResultSource.From (name, xs, id)

        static member From (name: ResultName, xs: _ ref array, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: Ref<_> array, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> Array.mapi (fun i x -> ResultSource.From (name + subname i, x, id))
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: Ref<_> array, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: Var<_> array, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> Array.mapi (fun i x -> ResultSource.From (name + subname i, x, id))
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: Var<_> array, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: Queue<_> array, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> Array.mapi (fun i x -> ResultSource.From (name + subname i, x, id))
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: Queue<_> array, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: InfiniteQueue<_> array, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> Array.mapi (fun i x -> ResultSource.From (name + subname i, x, id))
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: InfiniteQueue<_> array, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: Server<_, _, _> array, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> Array.mapi (fun i x -> ResultSource.From (name + subname i, x, id))
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: Server<_, _, _> array, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: ArrivalTimer array, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> Array.mapi (fun i x -> ResultSource.From (name + subname i, x, id))
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: ArrivalTimer array, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: Resource array, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> Array.mapi (fun i x -> ResultSource.From (name + subname i, x, id))
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: Resource array, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)

        static member From (name: ResultName, xs: PreemptibleResource array, id: ResultId) =
            let subname (i: int) = ".[" + i.ToString () + "]"
            let items = xs |> Array.mapi (fun i x -> ResultSource.From (name + subname i, x, id))
            let subscript = items |> Array.mapi (fun i x -> subname i)
            BasicResultArray (name, items, subscript, id) :> ResultArray |> ResultArraySource

        static member From (name: ResultName, xs: PreemptibleResource array, ?descr: ResultDescription) =
            ResultSource.From (name, xs, CustomResultId descr)
                                                
        member x.WriteUsingLabel (w: TextWriter, indent: int, provider: IFormatProvider, label: ResultName) =
            eventive {

                let tab = String.replicate indent " "
                let provider = 
                    match provider.GetFormat (typeof<ResultId>) with
                    | :? ResultFormatInfo as z -> z
                    | _ -> failwithf "Cannot find ResultFormatInfo provider"

                match x with

                | ResultItemSource z ->

                    match provider.GetDescription (z.Id) with
                    | None -> ()
                    | Some t ->

                        w.Write (tab)
                        w.Write ("// ")
                        w.WriteLine (t)

                    let! a = (z.GetStringValue (provider)).Data

                    w.Write (tab)
                    w.Write (label)
                    w.Write (" = ")
                    w.WriteLine (a)
                    w.WriteLine ()

                | ResultArraySource z ->

                    match provider.GetDescription (z.Id) with
                    | None -> ()
                    | Some t ->

                        w.Write (tab)
                        w.Write ("// ")
                        w.WriteLine (t)

                    w.Write (tab)
                    w.Write (label)
                    w.WriteLine (':')
                    w.WriteLine ()

                    for i = 0 to z.Items.Length - 1 do

                        let source = z.Items.[i]
                        let label  = label + z.Subscript.[i]
                        let indent = indent + 2

                        do! source.WriteUsingLabel (w, indent, provider, label)

                | ResultObjectSource z ->

                    match provider.GetDescription (z.Id) with
                    | None -> ()
                    | Some t ->

                        w.Write (tab)
                        w.Write ("// ")
                        w.WriteLine (t)

                    w.Write (tab)
                    w.Write (label)
                    w.WriteLine (':')
                    w.WriteLine ()

                    for prop in z.Properties do

                        let source = prop.Source
                        let label  = prop.Label
                        let indent = indent + 2

                        do! source.WriteUsingLabel (w, indent, provider, label)

                | ResultSeparatorSource z ->

                    w.Write (tab)
                    w.Write (label)
                    w.WriteLine ()
                    w.WriteLine ()
            }

        member x.Write (w: TextWriter, indent: int, provider: IFormatProvider) =
            match x with
            | ResultItemSource z -> x.WriteUsingLabel (w, indent, provider, z.Name)
            | ResultObjectSource z -> x.WriteUsingLabel (w, indent, provider, z.Name)
            | ResultArraySource z -> x.WriteUsingLabel (w, indent, provider, z.Name)
            | ResultSeparatorSource z -> x.WriteUsingLabel (w, indent, provider, z.Name)

        member x.Write (w: TextWriter, indent: int) =
            x.Write (w, indent, ResultFormatInfo.CurrentInfo)

        member x.Write (w: TextWriter) =
            x.Write (w, 0)

type [<AbstractClass>] ResultSet () =
    abstract Sources: ResultSource list
   
/// Defines extension methods.
[<AutoOpen>]
module ResultSetExtensions =

    type ResultSet with

        member x.Write (w: TextWriter, indent: int, provider: IFormatProvider) =
            eventive {

                let x1 = ResultSource.text "----------"
                let x2 = ResultSource.From ("t", Dynamics.time, TimeId)
                
                do! x1.Write (w, indent, provider)
                do! x2.Write (w, indent, provider)

                for s in x.Sources do
                    do! s.Write (w, indent, provider)
            }

        member x.Write (w: TextWriter, indent: int) =
            x.Write (w, indent, ResultFormatInfo.CurrentInfo)

        member x.Write (w: TextWriter) =
            x.Write (w, 0)

        static member WriteInStartTime (w:TextWriter, specs:Specs, model:Simulation<ResultSet>, provider:IFormatProvider) =
            simulation {
                let! results = model
                return! results.Write (w, 0, provider)
                            |> Eventive.runInStartTime
            } |> Simulation.run specs

        static member WriteInStartTime (w:TextWriter, specs:Specs, model:Simulation<ResultSet>) =
            ResultSet.WriteInStartTime (w, specs, model, ResultFormatInfo.CurrentInfo)

        static member WriteInStopTime (w:TextWriter, specs:Specs, model:Simulation<ResultSet>, provider:IFormatProvider) =
            simulation {
                let! results = model
                return! results.Write (w, 0, provider)
                            |> Eventive.runInStopTime
            } |> Simulation.run specs

        static member WriteInStopTime (w:TextWriter, specs:Specs, model:Simulation<ResultSet>) =
            ResultSet.WriteInStopTime (w, specs, model, ResultFormatInfo.CurrentInfo)

        static member WriteInIntegTimes (w:TextWriter, specs:Specs, model:Simulation<ResultSet>, provider:IFormatProvider) =
            simulation {
                let! results = model
                let! ms = 
                    results.Write (w, 0, provider)
                            |> Eventive.run
                            |> Dynamics.runInIntegTimes
                for m in ms do
                    ignore m
            } |> Simulation.run specs

        static member WriteInIntegTimes (w:TextWriter, specs:Specs, model:Simulation<ResultSet>) =
            ResultSet.WriteInIntegTimes (w, specs, model, ResultFormatInfo.CurrentInfo)

        static member WriteInTimes (w:TextWriter, times:seq<float>, specs:Specs, model:Simulation<ResultSet>, provider:IFormatProvider) =
            simulation {
                let! results = model
                let! ms = 
                    results.Write (w, 0, provider)
                            |> Eventive.run
                            |> Dynamics.runInTimes times
                for m in ms do
                    ignore m
            } |> Simulation.run specs

        static member WriteInTimes (w:TextWriter, times:seq<float>, specs:Specs, model:Simulation<ResultSet>) =
            ResultSet.WriteInTimes (w, times, specs, model, ResultFormatInfo.CurrentInfo)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ResultSet =

    [<CompiledName ("Empty")>]
    let empty = 
            { new ResultSet () with
                override x.Sources = []
            }

    [<CompiledName ("Create")>]
    let create sources =
            { new ResultSet () with
                override x.Sources = sources
            }

    [<CompiledName ("Sources")>]
    let sources (results: ResultSet) = results.Sources

    [<CompiledName ("Summary")>]
    let summary results = results |> sources |> List.map ResultSource.summary |> create

    [<CompiledName ("Expand")>]
    let expand results = results |> sources |> List.map ResultSource.expand |> create

    [<CompiledName ("Signal")>]
    let signal results = results |> sources |> List.map ResultSource.signal |> ResultSignal.concat

    [<CompiledName ("Find")>]
    let find pred results =

        let rec loop = function
            | (ResultItemSource x) as z ->
                if pred z then [z] else []
            | (ResultObjectSource x) as z ->
                if pred z then [z] else x.Properties |> List.map (fun p -> p.Source) |> List.filter pred
            | (ResultArraySource x) as z ->
                if pred z then [z] else x.Items |> Array.toList |> List.map loop |> List.concat
            | (ResultSeparatorSource x) as z ->
                if pred z then [z] else []

        sources results |> List.map loop |> List.concat |> create
 
    [<CompiledName ("FindByName")>]
    let findByName name results = find (fun x -> ResultSource.name x = name) results
 
    [<CompiledName ("FindById")>]
    let findById id results = find (fun x -> ResultSource.id x = id) results
 
    [<CompiledName ("FindByTypeId")>]
    let findByTypeId typeId results = find (fun x -> ResultSource.typeId x = typeId) results

    [<CompiledName ("Items")>]
    let items results =

        let rec loop source = seq {
            match source with            
            | ResultItemSource x ->
                yield x
            | ResultObjectSource x -> 
                for p in x.Properties do
                    yield! loop p.Source
            | ResultArraySource x -> 
                for p in x.Items do
                    yield! loop p
            | ResultSeparatorSource x -> ()
        }

        seq {
            for source in sources results do
                yield! loop source
        }

    [<CompiledName ("IntValues")>]
    let intValues results = seq {
        for x in items results do
            yield x.GetIntValue () 
    }

    [<CompiledName ("IntListValues")>]
    let intListValues results = seq {
        for x in items results do
            yield x.GetIntListValue () 
    }

    [<CompiledName ("IntStatsValues")>]
    let intStatsValues results = seq {
        for x in items results do
            yield x.GetIntStatsValue () 
    }

    [<CompiledName ("IntStatsChoiceValues")>]
    let intStatsChoiceValues results = seq {
        for x in items results do
            yield x.GetIntStatsChoiceValue () 
    }

    [<CompiledName ("IntTimingStatsValues")>]
    let intTimingStatsValues results = seq {
        for x in items results do
            yield x.GetIntTimingStatsValue () 
    }

    [<CompiledName ("DoubleValues")>]
    let floatValues results = seq {
        for x in items results do
            yield x.GetDoubleValue () 
    }

    [<CompiledName ("DoubleListValues")>]
    let floatListValues results = seq {
        for x in items results do
            yield x.GetDoubleListValue () 
    }

    [<CompiledName ("DoubleStatsValues")>]
    let floatStatsValues results = seq {
        for x in items results do
            yield x.GetDoubleStatsValue () 
    }

    [<CompiledName ("DoubleStatsChoiceValues")>]
    let floatStatsChoiceValues results = seq {
        for x in items results do
            yield x.GetDoubleStatsChoiceValue () 
    }

    [<CompiledName ("DoubleTimingStatsValues")>]
    let floatTimingStatsValues results = seq {
        for x in items results do
            yield x.GetDoubleTimingStatsValue () 
    }

    [<CompiledName ("StringValues")>]
    let stringValues results = seq {
        for x in items results do
            yield x.GetStringValue () 
    }

    [<CompiledName ("FormatStringValues")>]
    let formatStringValues provider results = seq {
        for x in items results do
            yield x.GetStringValue (provider) 
    }

    [<CompiledName ("PrintInStartTime")>]
    let printInStartTime specs model =
        ResultSet.WriteInStartTime (System.Console.Out, specs, model)

    [<CompiledName ("PrintInStopTime")>]
    let printInStopTime specs model =
        ResultSet.WriteInStopTime (System.Console.Out, specs, model)

    [<CompiledName ("PrintInIntegTimes")>]
    let printInIntegTimes specs model =
        ResultSet.WriteInIntegTimes (System.Console.Out, specs, model)

    [<CompiledName ("PrintInTimes")>]
    let printInTimes times specs model =
        ResultSet.WriteInTimes (System.Console.Out, times, specs, model)
 
type ResultTransform = ResultSet -> ResultSet

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ResultTransform =

    [<CompiledName ("Empty")>]
    let empty = fun (results: ResultSet) -> ResultSet.empty

    [<CompiledName ("Append")>]
    let append (tr1: ResultTransform) (tr2: ResultTransform) =
        fun results ->
            let x1 = tr1 results |> ResultSet.sources
            let x2 = tr2 results |> ResultSet.sources
            let x  = x1 @ x2
            ResultSet.create x

    [<CompiledName ("Concat")>]
    let concat trs = trs |> List.fold append empty
