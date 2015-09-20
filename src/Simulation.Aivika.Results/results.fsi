
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

open Simulation.Aivika

/// A name used for indentifying the results when generating output.
type ResultName = string

/// A description of the simulation result.
type ResultDescription = string

/// Represents the very simulation results.
type ResultData<'e> = Eventive<'e>

/// The result source identifier.
type ResultId =
    /// The modeling time. 
    | TimeId
    /// A number of samples for the statistics.
    | SamplingStatsCountId
    /// The minimum value for the statistics.
    | SamplingStatsMinimumId
    /// The maximum value for the statistics.
    | SamplingStatsMaximumId
    /// The average value for the statistics.
    | SamplingStatsMeanId
    /// The average value of squares for the statistics.
    | SamplingStatsMean2Id
    /// The variance for the statistics.
    | SamplingStatsVarianceId
    /// The deviation for the statistics.
    | SamplingStatsDeviationId
    /// A number of samples for the timing statistics.
    | TimingStatsCountId
    /// The minimum value for the timing statistics.
    | TimingStatsMinimumId
    /// The maximum value for the timing statistics.
    | TimingStatsMaximumId
    /// The minimum time for the timing statistics.
    | TimingStatsMinimumTimeId
    /// The maximum time for the timing statistics.
    | TimingStatsMaximumTimeId
    /// The start time for the timing statistics.
    | TimingStatsStartTimeId
    /// The last time for the timing statistics.
    | TimingStatsLastTimeId
    /// The average value for the timing statistics.
    | TimingStatsMeanId
    /// The variance for the timing statistics.
    | TimingStatsVarianceId
    /// The deviation for the timing statistics.
    | TimingStatsDeviationId
    /// The weighted sum for the timing statistics.
    | TimingStatsSumId
    /// The weighted sum of squares for the timing statistics.
    | TimingStatsSum2Id
    /// The enqueueing strategy.
    | QueueInputStrategyId
    /// The queue storing strategy.
    | QueueStoringStrategyId
    /// The dequeueing strategy.
    | QueueOutputStrategyId
    /// Indicates whether the queue is empty.
    | QueueEmptyId
    /// Indicates whether the queue is full.
    | QueueFullId
    /// The queue capacity.
    | QueueMaxCountId
    /// The queue size.
    | QueueCountId
    /// The queue size statistics.
    | QueueCountStatsId
    /// A number of items that were enqueued or that we tried to enqueue.
    | QueueInputCountId
    /// A number of items that were lost when enqueueing.
    | QueueLostCountId
    /// A number of items that were stored when enqueuing.
    | QueueStoreCountId
    /// A number of times when we requested for dequeueing the items.
    | QueueOutputRequestCountId
    /// A number of items that were actually dequeued.
    | QueueOutputCountId
    /// A queue load factor.
    | QueueLoadFactorId
    /// The enqueuing rate.
    | QueueInputRateId
    /// The storing rate.
    | QueueStoreRateId
    /// The rate of requests for dequeueing.
    | QueueOutputRequestRateId
    /// The rate of extracted items from the queue.
    | QueueOutputRateId
    /// The wait time for the queue.
    | QueueWaitTimeId
    /// The total wait time for the queue.
    | QueueTotalWaitTimeId
    /// The enqueueing wait time.
    | QueueInputWaitTimeId
    /// The dequeueing wait time.
    | QueueOutputWaitTimeId
    /// The queue rate.
    | QueueRateId
    /// The initial server state.
    | ServerInitStateId
    /// The server state.
    | ServerStateId
    /// The total input wait time for the server.
    | ServerTotalInputWaitTimeId
    /// The total processing time for the server.
    | ServerTotalProcessingTimeId
    /// The total output wait time for the server.
    | ServerTotalOutputWaitTimeId
    /// The total preemption time for the server.
    | ServerTotalPreemptionTimeId
    /// The input wait time for the server.
    | ServerInputWaitTimeId
    /// The processing time for the server.
    | ServerProcessingTimeId
    /// The output wait time for the server.
    | ServerOutputWaitTimeId
    /// The preemption time for the server.
    | ServerPreemptionTimeId
    /// The input wait factor for the server.
    | ServerInputWaitFactorId
    /// The processing factor for the server.
    | ServerProcessingFactorId
    /// The output wait factor for the server.
    | ServerOutputWaitFactorId
    /// The preemption factor for the server.
    | ServerPreemptionFactorId
    /// The processing time of arrivals.
    | ArrivalProcessingTimeId
    /// The current available resource count.
    | ResourceCountId
    /// The available resource count statistics.
    | ResourceCountStatsId
    /// The current resource utilisation count.
    | ResourceUtilisationCountId
    /// The resource utilisation count statistics.
    | ResourceUtilisationCountStatsId
    /// The current queue length for the resource.
    | ResourceQueueCountId
    /// The queue length statistics for the resource.
    | ResourceQueueCountStatsId
    /// The total wait time for the resource.
    | ResourceTotalWaitTimeId
    /// The wait time for the resource.
    | ResourceWaitTimeId
    /// An user-defined result computation.
    | CustomResultId of ResultDescription option

/// The result type identifier.
type ResultTypeId = 
    /// No data.
    | EmptyId
    /// An integer number.
    | IntId
    /// A list of integer numbers.
    | IntListId
    /// The sampling statistics based on integer numbers.
    | IntSamplingStatsId
    /// The timing statistics based on double floating point numbers.
    | IntTimingStatsId
    /// A double floating point number.
    | DoubleId
    /// A list of double floating point numbers.
    | DoubleListId
    /// The sampling statistics based on integer numbers.
    | DoubleSamplingStatsId
    /// The timing statistics based on double floating point numbers.
    | DoubleTimingStatsId
    /// A string.
    | StringId
    /// An array of items.
    | ArrayId of ResultTypeId
    /// A separator.
    | SeparatorId
    /// A finite queue.
    | FiniteQueueId
    /// An infinite queue.
    | InfiniteQueueId
    /// A server.
    | ServerId
    /// The arrival timer.
    | ArrivalTimerId
    /// The resource.
    | ResourceId

/// The format info for the result source.
type [<AbstractClass>] ResultFormatInfo = 

    interface IFormatProvider

    /// Initializes a new instance.
    new: provider:IFormatProvider -> ResultFormatInfo

    /// Gets a description by the specified result source identifier.
    abstract GetDescription: id:ResultId -> ResultDescription option

    /// Gets a description by the specified result type identifier.
    abstract GetDescription: id:ResultTypeId -> ResultDescription option

    /// Returns the current format info.
    static member CurrentInfo: ResultFormatInfo

    /// Returns the invariant format info.
    static member InvariantInfo: ResultFormatInfo

/// Whether an object containing the results emits a signal notifying about change of data.
type ResultSignal =
    /// There is no signal at all.
    | EmptyResultSignal
    /// The signal is unknown, but the entity probably changes.
    | UnknownResultSignal
    /// When the signal is precisely specified.
    | ResultSignal of Signal<unit>
    /// When the specified signal was combined with unknown signal.
    | ResultSignalMix of Signal<unit>

/// This module contains functions for working with the result signals.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ResultSignal =

    /// An empty result signal
    [<CompiledName ("Empty")>]
    val empty: ResultSignal 

    /// Merges two result signals.
    [<CompiledName ("Merge")>]
    val merge: signal1:ResultSignal -> signal2:ResultSignal -> ResultSignal

    /// Concatenates the result signals.
    [<CompiledName ("Concat")>]
    val concat: signals:ResultSignal list -> ResultSignal

    /// Purifies the result signal based on predefined signals provided by the very model.
    [<CompiledName ("Purify")>]
    val purify: predefinedSignals:PredefinedSignalSet -> signal:ResultSignal -> Signal<unit>

/// A parameterised value that actually represents a generalised result item that have no parametric type.
type [<AbstractClass>] ResultValue<'a> =

    /// Initializes a new instance.
    new: unit -> ResultValue<'a> 

    /// Gets the value name.
    abstract Name: ResultName

    /// Gets the value identifier.
    abstract Id: ResultId

    /// Gets the actual data.
    abstract Data: ResultData<'a>
      
    /// Gets a signal.
    abstract Signal: ResultSignal

    /// Converts to an equal container.
    member ToContainer: unit -> ResultContainer<ResultData<'a>>

    /// Converts the container to an equal source of result values.
    static member FromContainer: container:ResultContainer<ResultData<'e>> -> ResultValue<'e>

    /// Create a result value by the specified data.
    static member FromData: name:ResultName * comp:ResultData<'e> * signal:ResultSignal * id:ResultId -> ResultValue<'e>

/// A container of the simulation results such as queue, server or array.
and [<AbstractClass>] ResultContainer<'a> =

    /// Initializes a new instance.
    new: unit -> ResultContainer<'a> 

    /// Gets the container name.
    abstract Name: ResultName

    /// Gets the container identifier.
    abstract Id: ResultId

    /// Gets the container itself.
    abstract Data: 'a

    /// Gets a signal.
    abstract Signal: ResultSignal

/// This module contains functions for working with the source of values.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ResultValue =

    /// Maps a function over a source of the result value.
    [<CompiledName ("Map")>]
    val map: f:('a -> 'b) -> value:ResultValue<'a> -> ResultValue<'b>

    /// Transforms the result value.
    [<CompiledName ("Ap")>]
    val ap: f:Eventive<'a -> 'b> -> value:ResultValue<'a> -> ResultValue<'b>

/// Specifies the result source.
type ResultSource =
    /// The source consisting of a single item
    | ResultItemSource of ResultItem
    /// An object-like source
    | ResultObjectSource of ResultObject
    /// An array-like source
    | ResultArraySource of ResultArray
    /// This is a separating text
    | ResultSeparatorSource of ResultSeparator
   
/// The simulation results are represented by a single item.
and [<AbstractClass>] ResultItem =

    /// Initializes a new instance.
    new: unit -> ResultItem 

    /// Gets the item name. 
    abstract Name: ResultName

    /// Gets the item identifier.
    abstract Id: ResultId

    /// Gets the item type identifier.
    abstract TypeId: ResultTypeId

    /// Gets the result signal.
    abstract Signal: ResultSignal

    /// Expands the result item.
    abstract Expand: unit -> ResultSource

    /// Returns the summary, a short version of the item.
    abstract Summary: unit -> ResultSource

    /// Converts to a source of integer numbers as possible.
    abstract TryGetIntValue: unit -> ResultValue<int> option

    /// Converts to a source of lists of integer numbers as possible.
    abstract TryGetIntListValue: unit -> ResultValue<int list> option

    /// Converts to a source of statistics based on integer numbers as possible.
    abstract TryGetIntStatsValue: unit -> ResultValue<SamplingStats<int>> option

    /// Converts to an optimised source of statistics based on integer numbers as possible.
    member TryGetIntStatsChoiceValue: unit -> ResultValue<Choice<int, SamplingStats<int>>> option

    /// Converts to a source of timing statistics based on integer numbers as possible.
    abstract TryGetIntTimingStatsValue: unit -> ResultValue<TimingStats<int>> option

    /// Converts to a source of double floating point numbers as possible.
    abstract TryGetDoubleValue: unit -> ResultValue<float> option

    /// Converts to a source of lists of double floating point numbers as possible.
    abstract TryGetDoubleListValue: unit -> ResultValue<float list> option

    /// Converts to a source of statistics based on double floating point numbers as possible.
    abstract TryGetDoubleStatsValue: unit -> ResultValue<SamplingStats<float>> option

    /// Converts to an optimised source of statistics based on double floating point numbers as possible.
    member TryGetDoubleStatsChoiceValue: unit -> ResultValue<Choice<float, SamplingStats<float>>> option

    /// Converts to a source of timing statistics based on double floating point numbers as possible.
    abstract TryGetDoubleTimingStatsValue: unit -> ResultValue<TimingStats<float>> option

    /// Converts to a source of strings as possible.
    abstract TryGetStringValue: unit -> ResultValue<string> option

    /// Converts to a source of strings as possible using the specified format provider.
    abstract TryGetStringValue: provider:IFormatProvider -> ResultValue<string> option

    /// Converts to a source of integer numbers.
    member GetIntValue: unit -> ResultValue<int>

    /// Converts to a source of lists of integer numbers.
    member GetIntListValue: unit -> ResultValue<int list>

    /// Converts to a source of statistics based on integer numbers.
    member GetIntStatsValue: unit -> ResultValue<SamplingStats<int>>

    /// Converts to an optimised source of statistics based on integer numbers.
    member GetIntStatsChoiceValue: unit -> ResultValue<Choice<int, SamplingStats<int>>>

    /// Converts to a source of timing statistics based on integer numbers.
    member GetIntTimingStatsValue: unit -> ResultValue<TimingStats<int>>

    /// Converts to a source of double floating point numbers.
    member GetDoubleValue: unit -> ResultValue<float>

    /// Converts to a source of lists of double floating point numbers.
    member GetDoubleListValue: unit -> ResultValue<float list>

    /// Converts to a source of statistics based on double floating point numbers.
    member GetDoubleStatsValue: unit -> ResultValue<SamplingStats<float>>

    /// Converts to an optimised source of statistics based on double floating point numbers.
    member GetDoubleStatsChoiceValue: unit -> ResultValue<Choice<float, SamplingStats<float>>>

    /// Converts to a source of timing statistics based on double floating point numbers.
    member GetDoubleTimingStatsValue: unit -> ResultValue<TimingStats<float>>

    /// Converts to a source of strings.
    member GetStringValue: unit -> ResultValue<string>

    /// Converts to a source of strings using the specified format provider.
    member GetStringValue: provider:IFormatProvider -> ResultValue<string>

/// The simulation results are represented by an object having properties.
and [<AbstractClass>] ResultObject =

    /// Initializes a new instance.
    new: unit -> ResultObject

    /// Gets the object name.
    abstract Name: ResultName

    /// Gets the object identifier.
    abstract Id: ResultId

    /// Gets the object type identifier.
    abstract TypeId: ResultTypeId

    /// Gets the properties.
    abstract Properties: ResultProperty list

    /// Gets a signal.
    abstract Signal: Lazy<ResultSignal>

    /// Returns the summary.
    abstract Summary: unit -> ResultSource

/// The object property containing the simulation results.
and [<AbstractClass>] ResultProperty =

    /// Initializes a new instance.
    new: unit -> ResultProperty

    /// Gets the property short label.
    abstract Label: ResultName

    /// Gets the property identifier.
    abstract Id: ResultId

    /// Gets the simulation results supplied by the property.
    abstract Source: ResultSource

/// The simulation results represented by an array.
and [<AbstractClass>] ResultArray =

    /// Initializes a new instance. 
    new: unit -> ResultArray

    /// Gets the array name.
    abstract Name: ResultName

    /// Gets the array identifier.
    abstract Id: ResultId

    /// Gets the array type identifier.
    abstract TypeId: ResultTypeId

    /// Gets the array items.
    abstract Items: ResultSource []

    /// Gets a subscript.
    abstract Subscript: ResultName []

    /// Gets a signal.
    abstract Signal: Lazy<ResultSignal>

    /// Returns the summary.
    abstract Summary: unit -> ResultSource

/// It separates the simulation results when printing.
and [<AbstractClass>] ResultSeparator =

    /// Initializes a new instance.
    new: unit -> ResultSeparator

    /// Gets the separator name.
    abstract Name: ResultName

    /// Gets the separator identifier.
    abstract Id: ResultId

    /// Gets the separator type identifier.
    abstract TypeId: ResultTypeId

/// This module contains functions for working with the result signals.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ResultSource =

    /// Returns the result source name.
    [<CompiledName ("Name")>]
    val name: source:ResultSource -> ResultName 

    /// Returns the result source identifier.
    [<CompiledName ("Id")>]
    val id: source:ResultSource -> ResultId 

    /// Returns the result source type identifier.
    [<CompiledName ("TypeId")>]
    val typeId: source:ResultSource -> ResultTypeId

    /// Returns a signal for the specified result source.
    [<CompiledName ("Signal")>]
    val signal: source:ResultSource -> ResultSignal 

    /// Returns the summary for the specified result source.
    [<CompiledName ("Summary")>]
    val summary: source:ResultSource -> ResultSource 

    /// Expands the specified result source.
    [<CompiledName ("Expand")>]
    val expand: source:ResultSource -> ResultSource 

    /// Returns a text separator.
    [<CompiledName ("Text")>]
    val text: name:string -> ResultSource

    /// Returns an empty object without properties.
    [<CompiledName ("Empty")>]
    val empty: name:ResultName -> id:ResultId -> ResultSource

/// Defines extension methods.
[<AutoOpen>]
module ResultObjectExtensions =

    type ResultObject with

        /// Expands the object.
        member Expand: unit -> ResultSource

/// Defines extension methods.
[<AutoOpen>]
module ResultArrayExtensions =

    type ResultArray with

        /// Expands the array.
        member Expand: unit -> ResultSource

/// Defines extension methods.
[<AutoOpen>]
module ResultPropertyExtensions =

    type ResultProperty with

        /// Converts the computation to a result property.
        static member FromIntConst: label:ResultName * comp:ResultContainer<'a> * f:('a -> int) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromInt: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<int>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromInt: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<int>) * g:('a -> Signal<unit>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromInt: label:ResultName * comp:ResultContainer<ResultData<'a>> * f:('a -> int) * signal:ResultSignal * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromIntListConst: label:ResultName * comp:ResultContainer<'a> * f:('a -> int list) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromIntList: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<int list>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromIntList: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<int list>) * g:('a -> Signal<unit>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromIntList: label:ResultName * comp:ResultContainer<ResultData<'a>> * f:('a -> int list) * signal:ResultSignal * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromIntStatsConst: label:ResultName * comp:ResultContainer<'a> * f:('a -> SamplingStats<int>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromIntStats: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<SamplingStats<int>>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromIntStats: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<SamplingStats<int>>) * g:('a -> Signal<unit>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromIntStats: label:ResultName * comp:ResultContainer<ResultData<'a>> * f:('a -> SamplingStats<int>) * signal:ResultSignal * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromIntTimingStatsConst: label:ResultName * comp:ResultContainer<'a> * f:('a -> TimingStats<int>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromIntTimingStats: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<TimingStats<int>>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromIntTimingStats: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<TimingStats<int>>) * g:('a -> Signal<unit>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromIntTimingStats: label:ResultName * comp:ResultContainer<ResultData<'a>> * f:('a -> TimingStats<int>) * signal:ResultSignal * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromDoubleConst: label:ResultName * comp:ResultContainer<'a> * f:('a -> float) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromDouble: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<float>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromDouble: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<float>) * g:('a -> Signal<unit>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromDouble: label:ResultName * comp:ResultContainer<ResultData<'a>> * f:('a -> float) * signal:ResultSignal * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromDoubleListConst: label:ResultName * comp:ResultContainer<'a> * f:('a -> float list) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromDoubleList: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<float list>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromDoubleList: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<float list>) * g:('a -> Signal<unit>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromDoubleList: label:ResultName * comp:ResultContainer<ResultData<'a>> * f:('a -> float list) * signal:ResultSignal * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromDoubleStatsConst: label:ResultName * comp:ResultContainer<'a> * f:('a -> SamplingStats<float>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromDoubleStats: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<SamplingStats<float>>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromDoubleStats: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<SamplingStats<float>>) * g:('a -> Signal<unit>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromDoubleStats: label:ResultName * comp:ResultContainer<ResultData<'a>> * f:('a -> SamplingStats<float>) * signal:ResultSignal * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromDoubleTimingStatsConst: label:ResultName * comp:ResultContainer<'a> * f:('a -> TimingStats<float>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromDoubleTimingStats: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<TimingStats<float>>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromDoubleTimingStats: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<TimingStats<float>>) * g:('a -> Signal<unit>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromDoubleTimingStats: label:ResultName * comp:ResultContainer<ResultData<'a>> * f:('a -> TimingStats<float>) * signal:ResultSignal * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromStringConst: label:ResultName * comp:ResultContainer<'a> * f:('a -> string) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromString: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<string>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromString: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<string>) * g:('a -> Signal<unit>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromString: label:ResultName * comp:ResultContainer<ResultData<'a>> * f:('a -> string) * signal:ResultSignal * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromBooleanConst: label:ResultName * comp:ResultContainer<'a> * f:('a -> bool) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromBoolean: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<bool>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromBoolean: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<bool>) * g:('a -> Signal<unit>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromBoolean: label:ResultName * comp:ResultContainer<ResultData<'a>> * f:('a -> bool) * signal:ResultSignal * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromAnyConst: label:ResultName * comp:ResultContainer<'a> * f:('a -> 'b) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromAny: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<'b>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromAny: label:ResultName * comp:ResultContainer<'a> * f:('a -> Eventive<'b>) * g:('a -> Signal<unit>) * id:ResultId -> ResultProperty

        /// Converts the computation to a result property.
        static member FromAny: label:ResultName * comp:ResultContainer<ResultData<'a>> * f:('a -> 'b) * signal:ResultSignal * id:ResultId -> ResultProperty

/// Defines extension methods.
[<AutoOpen>]
module ResultSourceExtensions =

    type ResultSource with

        /// Gets the result signal.
        member Signal: ResultSignal

        /// Returns the summary.
        member Summary: unit -> ResultSource

        /// Converts the array to a result source.
        static member From: name:ResultName * items:ResultSource array * subscript:ResultName array * id:ResultId -> ResultSource

        /// Converts the array to a result source.
        static member From: name:ResultName * items:ResultSource array * subscript:ResultName array * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Parameter<'a> * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Parameter<'a> * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Simulation<'a> * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Simulation<'a> * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Dynamics<'a> * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Dynamics<'a> * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Eventive<'a> * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Eventive<'a> * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:ISignalable<'a> * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:ISignalable<'a> * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:'a ref * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:'a ref * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Ref<'a> * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Ref<'a> * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Var<'a> * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Var<'a> * ?descr:ResultDescription -> ResultSource

        /// Converts the queue to a result source.
        static member From: name:ResultName * comp:Queue<'a> * id:ResultId -> ResultSource

        /// Converts the queue to a result source.
        static member From: name:ResultName * comp:Queue<'a> * ?descr:ResultDescription -> ResultSource

        /// Converts the infinite queue to a result source.
        static member From: name:ResultName * comp:InfiniteQueue<'a> * id:ResultId -> ResultSource

        /// Converts the infinite queue to a result source.
        static member From: name:ResultName * comp:InfiniteQueue<'a> * ?descr:ResultDescription -> ResultSource

        /// Converts the server to a result source.
        static member From: name:ResultName * comp:Server<'s, 'a, 'b> * id:ResultId -> ResultSource

        /// Converts the server to a result source.
        static member From: name:ResultName * comp:Server<'s, 'a, 'b> * ?descr:ResultDescription -> ResultSource

        /// Converts the server to a result source.
        static member From: name:ResultName * comp:ArrivalTimer * id:ResultId -> ResultSource

        /// Converts the server to a result source.
        static member From: name:ResultName * comp:ArrivalTimer * ?descr:ResultDescription -> ResultSource

        /// Converts the server to a result source.
        static member From: name:ResultName * comp:Resource * id:ResultId -> ResultSource

        /// Converts the server to a result source.
        static member From: name:ResultName * comp:Resource * ?descr:ResultDescription -> ResultSource

        /// Converts the server to a result source.
        static member From: name:ResultName * comp:PreemptibleResource * id:ResultId -> ResultSource

        /// Converts the server to a result source.
        static member From: name:ResultName * comp:PreemptibleResource * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Parameter<'a> list * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Parameter<'a> list * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Simulation<'a> list * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Simulation<'a> list * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Dynamics<'a> list * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Dynamics<'a> list * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Eventive<'a> list * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Eventive<'a> list * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:ISignalable<'a> list * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:ISignalable<'a> list * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:'a ref list * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:'a ref list * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Ref<'a> list * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Ref<'a> list * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Var<'a> list * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Var<'a> list * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Queue<'a> list * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Queue<'a> list * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:InfiniteQueue<'a> list * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:InfiniteQueue<'a> list * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Server<'s, 'a, 'b> list * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Server<'s, 'a, 'b> list * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:ArrivalTimer list * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:ArrivalTimer list * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Resource list * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Resource list * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:PreemptibleResource list * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:PreemptibleResource list * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Parameter<'a> array * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Parameter<'a> array * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Simulation<'a> array * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Simulation<'a> array * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Dynamics<'a> array * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Dynamics<'a> array * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Eventive<'a> array * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Eventive<'a> array * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:ISignalable<'a> array * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:ISignalable<'a> array * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:'a ref array * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:'a ref array * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Ref<'a> array * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Ref<'a> array * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Var<'a> array * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Var<'a> array * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Queue<'a> array * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Queue<'a> array * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:InfiniteQueue<'a> array * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:InfiniteQueue<'a> array * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Server<'s, 'a, 'b> array * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Server<'s, 'a, 'b> array * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:ArrivalTimer array * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:ArrivalTimer array * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Resource array * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:Resource array * ?descr:ResultDescription -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:PreemptibleResource array * id:ResultId -> ResultSource

        /// Converts the computation to a result source.
        static member From: name:ResultName * comp:PreemptibleResource array * ?descr:ResultDescription -> ResultSource

        /// Returns a computation that writes the results using the specified indent and format provider.
        member Write: writer:TextWriter * indent:int * provider:IFormatProvider -> Eventive<unit>

        /// Returns a computation that writes the results using the specified indent.
        member Write: writer:TextWriter * indent:int -> Eventive<unit>

        /// Returns a computation that writes the results.
        member Write: writer:TextWriter -> Eventive<unit>

/// It represents the result set.
type [<AbstractClass>] ResultSet =

    /// Initializes a new instance.
    new: unit -> ResultSet

    /// Returns a list of sources.
    abstract Sources: ResultSource list

/// Defines extension methods.
[<AutoOpen>]
module ResultSetExtensions =

    type ResultSet with

        /// Returns a computation that writes the results using the specified indent and format provider.
        member Write: writer:TextWriter * indent:int * provider:IFormatProvider -> Eventive<unit>

        /// Returns a computation that writes the results using the specified indent.
        member Write: writer:TextWriter * indent:int -> Eventive<unit>

        /// Returns a computation that writes the results.
        member Write: writer:TextWriter -> Eventive<unit>

        /// Runs a simulation and writes the results in start time.
        static member WriteInStartTime: writer:TextWriter * specs:Specs * model:Simulation<ResultSet> * provider:IFormatProvider -> unit

        /// Runs a simulation and writes the results in start time.
        static member WriteInStartTime: writer:TextWriter * specs:Specs * model:Simulation<ResultSet> -> unit

        /// Runs a simulation and writes the results in stop time.
        static member WriteInStopTime: writer:TextWriter * specs:Specs * model:Simulation<ResultSet> * provider:IFormatProvider -> unit

        /// Runs a simulation and writes the results in stop time.
        static member WriteInStopTime: writer:TextWriter * specs:Specs * model:Simulation<ResultSet> -> unit

        /// Runs a simulation and writes the results in the integration time points.
        static member WriteInIntegTimes: writer:TextWriter * specs:Specs * model:Simulation<ResultSet> * provider:IFormatProvider -> unit

        /// Runs a simulation and writes the results in the integration time points.
        static member WriteInIntegTimes: writer:TextWriter * specs:Specs * model:Simulation<ResultSet> -> unit

        /// Runs a simulation and writes the results in the specified time points.
        static member WriteInTimes: writer:TextWriter * times:seq<float> * specs:Specs * model:Simulation<ResultSet> * provider:IFormatProvider -> unit

        /// Runs a simulation and writes the results in the specified time points.
        static member WriteInTimes: writer:TextWriter * times:seq<float> * specs:Specs * model:Simulation<ResultSet> -> unit

/// This module contains functions for working with the result sets.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ResultSet =

    /// Returns an empty result set.
    [<CompiledName ("Empty")>]
    val empty: ResultSet

    /// Returns a result set by the specified list of sources.
    [<CompiledName ("Create")>]
    val create: sources:ResultSource list -> ResultSet 

    /// Returns a list of sources for the specfied result set.
    [<CompiledName ("Sources")>]
    val sources: results:ResultSet -> ResultSource list

    /// Returns the summary, shortening the result sources if needed.
    [<CompiledName ("Summary")>]
    val summary: results:ResultSet -> ResultSet

    /// Expand the result sources if needed.
    [<CompiledName ("Expand")>]
    val expand: results:ResultSet -> ResultSet

    /// Returns the signal.
    [<CompiledName ("Signal")>]
    val signal: results:ResultSet -> ResultSignal

    /// Finds a subset satisfying the specified predicate.
    [<CompiledName ("Find")>]
    val find: pred:(ResultSource -> bool) -> results:ResultSet -> ResultSet

    /// Finds a subset by the specified name.
    [<CompiledName ("FindByName")>]
    val findByName: name:ResultName -> results:ResultSet -> ResultSet

    /// Finds a subset by the specified identifier.
    [<CompiledName ("FindById")>]
    val findById: id:ResultId -> results:ResultSet -> ResultSet

    /// Finds a subset by the specified type identifier.
    [<CompiledName ("FindByTypeId")>]
    val findByTypeId: typeId:ResultTypeId -> results:ResultSet -> ResultSet

    /// Returns the items of the specified result set.
    [<CompiledName ("Items")>]
    val items: results:ResultSet -> seq<ResultItem>

    /// Returns the integer values by the specified the result set.
    [<CompiledName ("IntValues")>]
    val intValues: results:ResultSet -> seq<ResultValue<int>>

    /// Returns the lists of integer values by the specified the result set.
    [<CompiledName ("IntListValues")>]
    val intListValues: results:ResultSet -> seq<ResultValue<int list>>

    /// Returns the statistics based on integer values by the specified the result set.
    [<CompiledName ("IntStatsValues")>]
    val intStatsValues: results:ResultSet -> seq<ResultValue<SamplingStats<int>>>

    /// Returns the statistics based on integer values by the specified the result set.
    [<CompiledName ("IntStatsChoiceValues")>]
    val intStatsChoiceValues: results:ResultSet -> seq<ResultValue<Choice<int, SamplingStats<int>>>>

    /// Returns the timing statistics based on integer values by the specified the result set.
    [<CompiledName ("IntTimingStatsValues")>]
    val intTimingStatsValues: results:ResultSet -> seq<ResultValue<TimingStats<int>>>

    /// Returns the floating point values by the specified the result set.
    [<CompiledName ("DoubleValues")>]
    val floatValues: results:ResultSet -> seq<ResultValue<float>>

    /// Returns the lists of floating point values by the specified the result set.
    [<CompiledName ("DoubleListValues")>]
    val floatListValues: results:ResultSet -> seq<ResultValue<float list>>

    /// Returns the statistics based on floating point values by the specified the result set.
    [<CompiledName ("DoubleStatsValues")>]
    val floatStatsValues: results:ResultSet -> seq<ResultValue<SamplingStats<float>>>

    /// Returns the statistics based on floating point values by the specified the result set.
    [<CompiledName ("DoubleStatsChoiceValues")>]
    val floatStatsChoiceValues: results:ResultSet -> seq<ResultValue<Choice<float, SamplingStats<float>>>>

    /// Returns the timing statistics based on floating point values by the specified the result set.
    [<CompiledName ("DoubleTimingStatsValues")>]
    val floatTimingStatsValues: results:ResultSet -> seq<ResultValue<TimingStats<float>>>

    /// Returns the string values by the specified the result set.
    [<CompiledName ("StringValues")>]
    val stringValues: results:ResultSet -> seq<ResultValue<string>>

    /// Returns the string values by the specified format provider and result set.
    [<CompiledName ("FormatStringValues")>]
    val formatStringValues: provider:IFormatProvider -> results:ResultSet -> seq<ResultValue<string>>

    /// Tries to return the integer values by the specified the result set.
    [<CompiledName ("TryGetIntValues")>]
    val tryGetIntValues: results:ResultSet -> seq<ResultValue<int> option>

    /// Tries to return the lists of integer values by the specified the result set.
    [<CompiledName ("TryGetIntListValues")>]
    val tryGetIntListValues: results:ResultSet -> seq<ResultValue<int list> option>

    /// Tries to return the statistics based on integer values by the specified the result set.
    [<CompiledName ("TryGetIntStatsValues")>]
    val tryGetIntStatsValues: results:ResultSet -> seq<ResultValue<SamplingStats<int>> option>

    /// Tries to return the statistics based on integer values by the specified the result set.
    [<CompiledName ("TryGetIntStatsChoiceValues")>]
    val tryGetIntStatsChoiceValues: results:ResultSet -> seq<ResultValue<Choice<int, SamplingStats<int>>> option>

    /// Tries to return the timing statistics based on integer values by the specified the result set.
    [<CompiledName ("TryGetIntTimingStatsValues")>]
    val tryGetIntTimingStatsValues: results:ResultSet -> seq<ResultValue<TimingStats<int>> option>

    /// Tries to return the floating point values by the specified the result set.
    [<CompiledName ("TryGetDoubleValues")>]
    val tryGetFloatValues: results:ResultSet -> seq<ResultValue<float> option>

    /// Tries to return the lists of floating point values by the specified the result set.
    [<CompiledName ("TryGetDoubleListValues")>]
    val tryGetFloatListValues: results:ResultSet -> seq<ResultValue<float list> option>

    /// Tries to return the statistics based on floating point values by the specified the result set.
    [<CompiledName ("TryGetDoubleStatsValues")>]
    val tryGetFloatStatsValues: results:ResultSet -> seq<ResultValue<SamplingStats<float>> option>

    /// Tries to return the statistics based on floating point values by the specified the result set.
    [<CompiledName ("TryGetDoubleStatsChoiceValues")>]
    val tryGetFloatStatsChoiceValues: results:ResultSet -> seq<ResultValue<Choice<float, SamplingStats<float>>> option>

    /// Tries to return the timing statistics based on floating point values by the specified the result set.
    [<CompiledName ("TryGetDoubleTimingStatsValues")>]
    val tryGetFloatTimingStatsValues: results:ResultSet -> seq<ResultValue<TimingStats<float>> option>

    /// Tries to return the string values by the specified the result set.
    [<CompiledName ("TryGetStringValues")>]
    val tryGetStringValues: results:ResultSet -> seq<ResultValue<string> option>

    /// Tries to return the string values by the specified format provider and result set.
    [<CompiledName ("TryFormatStringValues")>]
    val tryFormatStringValues: provider:IFormatProvider -> results:ResultSet -> seq<ResultValue<string> option>

    /// Runs a simulation and prints the results in start time.
    [<CompiledName ("PrintInStartTime")>]
    val printInStartTime: specs:Specs -> model:Simulation<ResultSet> -> unit

    /// Runs a simulation and prints the results in stop time.
    [<CompiledName ("PrintInStopTime")>]
    val printInStopTime: specs:Specs -> model:Simulation<ResultSet> -> unit

    /// Runs a simulation and prints the results in the integration time points.
    [<CompiledName ("PrintInIntegTimes")>]
    val printInIntegTimes: specs:Specs -> model:Simulation<ResultSet> -> unit

    /// Runs a simulation and prints the results in the specified time points.
    [<CompiledName ("PrintInTimes")>]
    val printInTimes: times:seq<float> -> specs:Specs -> model:Simulation<ResultSet> -> unit

/// This is a result set transformation.
type ResultTransform = ResultSet -> ResultSet

/// This module contains functions for working with the result set transformations.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ResultTransform =

    /// A transformation that returns none results.
    [<CompiledName ("Empty")>]
    val empty: ResultTransform

    /// Appends two result set transformations.
    [<CompiledName ("Append")>]
    val append: tr1:ResultTransform -> tr2:ResultTransform -> ResultTransform

    /// Concatenates the result set transformations.
    [<CompiledName ("Concat")>]
    val concat: trs:ResultTransform list -> ResultTransform
