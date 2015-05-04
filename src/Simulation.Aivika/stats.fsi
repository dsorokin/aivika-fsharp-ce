
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
open System.Globalization
open System.IO

/// Describes an immutable summary for the statistics that consists only of samples not bound up with the simulation time.
[<AbstractClass>]
type SamplingStats<'a> =

    /// Initializes a new instance.
    new: unit -> SamplingStats<'a>

    /// Adds the specified sample producing another statistics.
    abstract Add: sample:'a -> SamplingStats<'a>

    /// Appends other statistics producing another one.
    abstract Append: stats:SamplingStats<'a> -> SamplingStats<'a>

    /// Appends other statistics producing another one with a room for opimising the first source.
    member AppendChoice: stats:Choice<'a, SamplingStats<'a>> -> SamplingStats<'a>

    /// Adds new samples to the immutable statistics producing a new statistics.
    member AppendSeq: samples:seq<'a> -> SamplingStats<'a>

    /// Returns the total number of samples.
    abstract Count: int

    /// The minimum value among the samples.
    abstract Minimum: 'a

    /// The maximum value among the samples.
    abstract Maximum: 'a

    /// Returns the average value.
    abstract Mean: float

    /// Returns the average square value.
    abstract Mean2: float

    /// Returns the variance.
    member Variance: float

    /// Returns the standard deviation.
    member Deviation: float

    /// Converts to a string.
    override ToString: unit -> string

    /// Converts to a string using the specified format provider.
    member ToString: provider:IFormatProvider -> string

/// This module defines some extensions for the sampling statistics.
[<AutoOpen>]
module SamplingStatsExtensions =

    type SamplingStats<'a> with

        /// Writes the statistics summary using the specified indent and format provider.
        member Write: writer:TextWriter * indent:int * provider:IFormatProvider -> unit

        /// Writes the statistics summary using the specified indent.
        member Write: writer:TextWriter * indent:int -> unit

        /// Writes the statistics summary.
        member Write: writer:TextWriter -> unit

/// This module contains helper functions for working with the sampling statistics.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module SamplingStats =

    /// An empty statistics that has no samples (of integers).
    [<CompiledName ("EmptyInt32s")>]
    val emptyInts: SamplingStats<int>

    /// An empty statistics that has no samples (of floating point numbers).
    [<CompiledName ("EmptyDoubles")>]
    val emptyFloats: SamplingStats<float>

    /// Returns statistics by the specified samples (of integers).
    [<CompiledName ("FromInt32s")>]
    val fromInts: samples:int array -> SamplingStats<int>

    /// Returns statistics by the specified samples (of floating point numbers).
    [<CompiledName ("FromDoubles")>]
    val fromFloats: samples:float array -> SamplingStats<float>

    /// Adds a new sample to the immutable statistics producing a new statistics.
    [<CompiledName ("Add")>]
    val add: sample:'a -> stats:SamplingStats<'a> -> SamplingStats<'a>

    /// Takes two immutable statistics and combines them producing a new one.
    [<CompiledName ("Append")>]
    val append: stats1:SamplingStats<'a> -> stats2:SamplingStats<'a> -> SamplingStats<'a>

    /// Takes two immutable statistics and combines them producing a new one with a room for optimising the first source.
    [<CompiledName ("AppendChoice")>]
    val appendChoice: stats1:Choice<'a, SamplingStats<'a>> -> stats2:SamplingStats<'a> -> SamplingStats<'a>

    /// Adds new samples to the immutable statistics producing a new statistics.
    [<CompiledName ("AppendSeq")>]
    val appendSeq: samples:seq<'a> -> stats:SamplingStats<'a> -> SamplingStats<'a>

    /// Returns the total number of samples.
    [<CompiledName ("Count")>]
    val count: stats:SamplingStats<'a> -> int

    /// The minimum value among the samples.
    [<CompiledName ("Minimum")>]
    val minimum: stats:SamplingStats<'a> -> 'a

    /// The maximum value among the samples.
    [<CompiledName ("Maximum")>]
    val maximum: stats:SamplingStats<'a> -> 'a

    /// Returns the average value.
    [<CompiledName ("Mean")>]
    val mean: stats:SamplingStats<'a> -> float

    /// Returns the average square value.
    [<CompiledName ("Mean2")>]
    val mean2: stats:SamplingStats<'a> -> float

    /// Returns the variance.
    [<CompiledName ("Variance")>]
    val variance: stats:SamplingStats<'a> -> float

    /// Returns the standard deviation.
    [<CompiledName ("Deviation")>]
    val deviation: stats:SamplingStats<'a> -> float

    /// Convert the statistics from integer to double values.
    [<CompiledName ("FromInt32sToDoubles")>]
    val fromIntsToFloats: stats:SamplingStats<int> -> SamplingStats<float>

/// Describes an immutable summary for the statistics that consists of samples assigned to the simulation time points.
[<AbstractClass>]
type TimingStats<'a> =

    /// Initializes a new instance.
    new: unit -> TimingStats<'a>

    /// Adds a new sample assigned to the specified simulation time to produce another statistics.
    abstract Add: time:Time * sample:'a -> TimingStats<'a>

    /// Returns the total number of samples.
    abstract Count: int

    /// The minimum value among the samples.
    abstract Minimum: 'a

    /// The maximum value among the samples.
    abstract Maximum: 'a

    /// The last value.
    abstract Last: 'a

    /// Returns the simulation time at which the minimum was attained.
    abstract MinimumTime: Time

    /// Returns the simulation time at which the maximum was attained.
    abstract MaximumTime: Time

    /// Returns the start time of sampling.
    abstract StartTime: Time

    /// Returns the last time of sampling.
    abstract LastTime: Time

    /// Returns the weighted sum of values.
    abstract Sum: float

    /// Returns the weighted sum of square values.
    abstract Sum2: float

    /// Returns the average value.
    abstract Mean: float

    /// Returns the average square value.
    abstract Mean2: float

    /// Returns the variance.
    member Variance: float

    /// Returns the standard deviation.
    member Deviation: float

    /// Converts to a string.
    override ToString: unit -> string

    /// Converts to a string using the specified format provider.
    member ToString: provider:IFormatProvider -> string

    /// Converts to the sampling statistics normalised by the specified count.
    abstract Normalise: count:int -> SamplingStats<'a>

/// This module defines some extensions for the timing statistics.
[<AutoOpen>]
module TimingStatsExtensions =

    type TimingStats<'a> with

        /// Writes the statistics summary using the specified indent and format provider.
        member Write: writer:TextWriter * indent:int * provider:IFormatProvider -> unit

        /// Writes the statistics summary using the specified indent.
        member Write: writer:TextWriter * indent:int -> unit

        /// Writes the statistics summary.
        member Write: writer:TextWriter -> unit

/// This module contains helper functions for working with the timing statistics.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module TimingStats =

    /// An empty statistics that has no samples (of integers).
    [<CompiledName ("EmptyInt32s")>]
    val emptyInts: TimingStats<int>

    /// An empty statistics that has no samples (of floating point numbers).
    [<CompiledName ("EmptyDoubles")>]
    val emptyFloats: TimingStats<float>

    /// Adds a new sample assigned to some simulation time and produces a new statistics.
    [<CompiledName ("Add")>]
    val add: time:Time -> sample:'a -> stats:TimingStats<'a> -> TimingStats<'a>

    /// Returns the total number of samples.
    [<CompiledName ("Count")>]
    val count: stats:TimingStats<'a> -> int

    /// The minimum value among the samples.
    [<CompiledName ("Minimum")>]
    val minimum: stats:TimingStats<'a> -> 'a

    /// The maximum value among the samples.
    [<CompiledName ("Maximum")>]
    val maximum: stats:TimingStats<'a> -> 'a

    /// The last value.
    [<CompiledName ("Last")>]
    val last: stats:TimingStats<'a> -> 'a

    /// Returns the simulation time at which the minimum was attained.
    [<CompiledName ("MinimumTime")>]
    val minimumTime: stats:TimingStats<'a> -> Time

    /// Returns the simulation time at which the maximum was attained.
    [<CompiledName ("MaximumTime")>]
    val maximumTime: stats:TimingStats<'a> -> Time

    /// Returns the start time of sampling.
    [<CompiledName ("StartTime")>]
    val startTime: stats:TimingStats<'a> -> Time

    /// Returns the last time of sampling.
    [<CompiledName ("LastTime")>]
    val lastTime: stats:TimingStats<'a> -> Time

    /// Returns the weighted sum of values.
    [<CompiledName ("Sum")>]
    val sum: stats:TimingStats<'a> -> float

    /// Returns the weighted sum of square values.
    [<CompiledName ("Sum2")>]
    val sum2: stats:TimingStats<'a> -> float

    /// Returns the average value.
    [<CompiledName ("Mean")>]
    val mean: stats:TimingStats<'a> -> float

    /// Returns the average square value.
    [<CompiledName ("Mean2")>]
    val mean2: stats:TimingStats<'a> -> float

    /// Returns the variance.
    [<CompiledName ("Variance")>]
    val variance: stats:TimingStats<'a> -> float

    /// Returns the standard deviation.
    [<CompiledName ("Deviation")>]
    val deviation: stats:TimingStats<'a> -> float

    /// Converts the statistics from integer to double values.
    [<CompiledName ("FromInt32sToDoubles")>]
    val fromIntsToFloats: stats:TimingStats<int> -> TimingStats<float>

    /// Converts the statistics to another sample-based representation normalised by the specified count.
    [<CompiledName ("Normalise")>]
    val normalise: count:int -> stats:TimingStats<'a> -> SamplingStats<'a>
