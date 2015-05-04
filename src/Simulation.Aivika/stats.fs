
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
open System.IO
open System.Globalization

[<AbstractClass>]
type SamplingStats<'a> () =

    member x.ToString (provider: IFormatProvider) =

        "{ count = " + Convert.ToString (x.Count, provider) + 
        "; mean = " + Convert.ToString (x.Mean, provider) +
        "; std = " + Convert.ToString (x.Deviation, provider) +
        "; min = " + Convert.ToString (box x.Minimum, provider) +
        "; max = " + Convert.ToString (box x.Maximum, provider) +
        " }"

    override x.ToString () = x.ToString (NumberFormatInfo.CurrentInfo)

    abstract Add: 'a -> SamplingStats<'a>

    abstract Append: SamplingStats<'a> -> SamplingStats<'a>

    member x.AppendChoice (y: Choice<_, SamplingStats<_>>) =
        match y with
        | Choice1Of2 a -> x.Add (a)
        | Choice2Of2 a -> x.Append (a)

    member x.AppendSeq (samples) =
        Seq.fold (fun (st: SamplingStats<_>) a -> st.Add (a)) x samples

    abstract Count: int

    abstract Minimum: 'a

    abstract Maximum: 'a

    abstract Mean: float

    abstract Mean2: float

    member x.Variance: float =
        if x.Count = 0 then nan
        elif x.Count = 1 then 0.0
        else (x.Mean2 - x.Mean * x.Mean) * (float x.Count / (float x.Count - 1.0))

    member x.Deviation: float =
        sqrt x.Variance

[<Sealed>]
type IntSamplingStats (count, minimum, maximum, mean, mean2) =

    inherit SamplingStats<int> ()

    static let empty =
        IntSamplingStats (0, Int32.MaxValue, Int32.MinValue, nan, nan)

    static member Empty = empty :> SamplingStats<_>

    override x.Count = count

    override x.Minimum = minimum

    override x.Maximum = maximum

    override x.Mean = mean

    override x.Mean2 = mean2

    override x.Add (a) =
        if count = 0 then
            IntSamplingStats (1, a, a, float a, float <| a * a) :> SamplingStats<_>
        // elif Double.IsNaN (a) then
        //     x :> SamplingStats<_>
        else
            let count = 1 + count
            let minimum = min a minimum
            let maximum = max a maximum
            let n = float count
            let x = float a
            let k1 = 1.0 / n
            let k2 = (n - 1.0) / n
            let mean = k1 * x + k2 * mean
            let mean2 = k1 * x * x + k2 * mean2
            IntSamplingStats (count, minimum, maximum, mean, mean2) :> SamplingStats<_>

    override x.Append (y) =
        if count = 0 then
            y
        else if y.Count = 0 then
            x :> SamplingStats<_>
        else
            let count = y.Count + count
            let minimum = min y.Minimum minimum
            let maximum = max y.Maximum maximum
            let n = float count
            let k1 = (float x.Count) / n
            let k2 = (float y.Count) / n
            let mean = k1 * x.Mean + k2 * y.Mean
            let mean2 = k1 * x.Mean2 + k2 * y.Mean2
            IntSamplingStats (count, minimum, maximum, mean, mean2) :> SamplingStats<_>

    member x.ToDoubleStats () =
        DoubleSamplingStats (count, float minimum, float maximum, mean, mean2)
             :> SamplingStats<_>

    static member From (xs: int array) =
        let count = xs.Length 
        if count > 0 then
            let mutable minimum = xs.[0]
            let mutable maximum = xs.[0]
            let mutable mean  = float xs.[0]
            let mutable mean2 = mean * mean
            for i = 1 to count - 1 do
                let n = float (1 + i)
                let x = float xs.[i]
                let k1 = 1.0 / n
                let k2 = (n - 1.0) / n
                minimum <- min xs.[i] minimum
                maximum <- max xs.[i] maximum
                mean  <- k1 * x + k2 * mean
                mean2 <- k1 * x * x + k2 * mean2 
            IntSamplingStats (count, minimum, maximum, mean, mean2) :> SamplingStats<_>
        else
            IntSamplingStats.Empty

and [<Sealed>] DoubleSamplingStats (count, minimum, maximum, mean, mean2) =

    inherit SamplingStats<float> ()

    static let empty = 
        DoubleSamplingStats (0, infinity, -infinity, nan, nan)

    static member Empty = empty :> SamplingStats<_>

    override x.Count = count

    override x.Minimum = minimum

    override x.Maximum = maximum

    override x.Mean = mean

    override x.Mean2 = mean2

    override x.Add (a) =
        if count = 0 then
            DoubleSamplingStats (1, a, a, a, a * a) :> SamplingStats<_>
        elif Double.IsNaN (a) then
            x :> SamplingStats<_>
        else
            let count = 1 + count
            let minimum = min a minimum
            let maximum = max a maximum
            let n = float count
            let x = a
            let k1 = 1.0 / n
            let k2 = (n - 1.0) / n
            let mean = k1 * x + k2 * mean
            let mean2 = k1 * x * x + k2 * mean2
            DoubleSamplingStats (count, minimum, maximum, mean, mean2) :> SamplingStats<_>

    override x.Append (y) =
        if count = 0 then
            y
        else if y.Count = 0 then
            x :> SamplingStats<_>
        else
            let count = y.Count + count
            let minimum = min y.Minimum minimum
            let maximum = max y.Maximum maximum
            let n = float count
            let k1 = (float x.Count) / n
            let k2 = (float y.Count) / n
            let mean = k1 * x.Mean + k2 * y.Mean
            let mean2 = k1 * x.Mean2 + k2 * y.Mean2
            DoubleSamplingStats (count, minimum, maximum, mean, mean2) :> SamplingStats<_>

    static member From (xs: float array) =
        let count = xs.Length 
        if count > 0 then
            let mutable minimum = xs.[0]
            let mutable maximum = xs.[0]
            let mutable mean  = xs.[0]
            let mutable mean2 = mean * mean
            for i = 1 to count - 1 do
                let n = float (1 + i)
                let x = float xs.[i]
                let k1 = 1.0 / n
                let k2 = (n - 1.0) / n
                minimum <- min xs.[i] minimum
                maximum <- max xs.[i] maximum
                mean  <- k1 * x + k2 * mean
                mean2 <- k1 * x * x + k2 * mean2 
            DoubleSamplingStats (count, minimum, maximum, mean, mean2) :> SamplingStats<_>
        else
            DoubleSamplingStats.Empty

[<AutoOpen>]
module SamplingStatsExtensions =

    type SamplingStats<'a> with

        member x.Write (w: TextWriter, indent: int, provider: IFormatProvider) =

            let tab = String.replicate indent " "

            w.Write (tab)
            w.Write ("count = ")
            w.WriteLine (Convert.ToString (x.Count, provider))
        
            w.Write (tab)
            w.Write ("mean = ")
            w.WriteLine (Convert.ToString (x.Mean, provider))
        
            w.Write (tab)
            w.Write ("std = ")
            w.WriteLine (Convert.ToString (x.Deviation, provider))

            w.Write (tab)
            w.Write ("min = ")
            w.WriteLine (Convert.ToString (box x.Minimum, provider))

            w.Write (tab)
            w.Write ("max = ")
            w.WriteLine (Convert.ToString (box x.Maximum, provider))

        member x.Write (w: TextWriter, indent: int) =
            x.Write (w, indent, NumberFormatInfo.CurrentInfo)

        member x.Write (w: TextWriter) =
            x.Write (w, 0)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module SamplingStats =

    [<CompiledName ("EmptyInt32s")>]
    let emptyInts = IntSamplingStats.Empty

    [<CompiledName ("EmptyDoubles")>]
    let emptyFloats = DoubleSamplingStats.Empty

    [<CompiledName ("FromInt32s")>]
    let fromInts (xs: array<_>) = IntSamplingStats.From (xs)

    [<CompiledName ("FromDoubles")>]
    let fromFloats (xs: array<_>) = DoubleSamplingStats.From (xs)

    [<CompiledName ("Add")>]
    let add a (x: SamplingStats<_>) = x.Add (a)

    [<CompiledName ("Append")>]
    let append (x: SamplingStats<_>) (y: SamplingStats<_>) = y.Append (x)

    [<CompiledName ("AppendChoice")>]
    let appendChoice (x: Choice<_, SamplingStats<_>>) (y: SamplingStats<_>) = y.AppendChoice (x)

    [<CompiledName ("AppendSeq")>]
    let appendSeq xs (y: SamplingStats<_>) = y.AppendSeq (xs)

    [<CompiledName ("Count")>]
    let count (x: SamplingStats<_>) = x.Count

    [<CompiledName ("Minimum")>]
    let minimum (x: SamplingStats<_>) = x.Minimum

    [<CompiledName ("Maximum")>]
    let maximum (x: SamplingStats<_>) = x.Maximum

    [<CompiledName ("Mean")>]
    let mean (x: SamplingStats<_>) = x.Mean

    [<CompiledName ("Mean2")>]
    let mean2 (x: SamplingStats<_>) =x.Mean2

    [<CompiledName ("Variance")>]
    let variance (x: SamplingStats<_>) = x.Variance

    [<CompiledName ("Deviation")>]
    let deviation (x: SamplingStats<_>) = x.Deviation

    [<CompiledName ("FromInt32sToDoubles")>]
    let fromIntsToFloats (x: SamplingStats<int>) = 
        match x with
        | :? IntSamplingStats as z -> z.ToDoubleStats ()
        | _ -> failwithf "Expected to get the integer statistics"

[<AbstractClass>]
type TimingStats<'a> () =

    member x.ToString (provider: IFormatProvider) =

        "{ count = " + Convert.ToString (x.Count, provider) + 
        "; mean = " + Convert.ToString (x.Mean, provider) +
        "; std = " + Convert.ToString (x.Deviation, provider) +
        "; min = " + Convert.ToString (box x.Minimum, provider) +
        " (t = " + Convert.ToString (x.MinimumTime, provider) +
        "); max = " + Convert.ToString (box x.Maximum, provider) +
        " (t = " + Convert.ToString (x.MaximumTime, provider) +
        "); t in [" + Convert.ToString (x.StartTime, provider) +
        "; " + Convert.ToString (x.LastTime, provider) +
        "] }"

    override x.ToString () = x.ToString (NumberFormatInfo.CurrentInfo)

    abstract Add: time:Time * sample:'a -> TimingStats<'a>

    abstract Count: int

    abstract Minimum: 'a

    abstract Maximum: 'a

    abstract Last: 'a

    abstract MinimumTime: Time

    abstract MaximumTime: Time

    abstract StartTime: Time

    abstract LastTime: Time

    abstract Mean: float

    abstract Mean2: float

    abstract Sum: float

    abstract Sum2: float

    member x.Variance: float = x.Mean2 - x.Mean * x.Mean

    member x.Deviation: float = sqrt x.Variance

    abstract Normalise: count:int -> SamplingStats<'a>

[<Sealed>]
type IntTimingStats (count, minimum, maximum, last, timeOfMinimum, timeOfMaximum,
                        startTime, lastTime, sum, sum2) =

    inherit TimingStats<int> ()

    static let empty =
        IntTimingStats (0, Int32.MaxValue, Int32.MinValue, 0, infinity, -infinity,
            infinity, -infinity, nan, nan) 

    static member Empty = empty :> TimingStats<_>

    override x.Count = count

    override x.Minimum = minimum

    override x.Maximum = maximum

    override x.Last = last

    override x.MinimumTime = timeOfMinimum

    override x.MaximumTime = timeOfMaximum

    override x.StartTime = startTime

    override x.LastTime = lastTime

    override x.Sum = sum

    override x.Sum2 = sum2

    override x.Mean =
        if x.Count = 0 then 
            nan
        elif x.LastTime > x.StartTime then
            x.Sum / (x.LastTime - x.StartTime)
        else
            float x.Minimum

    override x.Mean2 =
        if x.Count = 0 then 
            nan
        elif x.LastTime > x.StartTime then
            x.Sum2 / (x.LastTime - x.StartTime)
        else
            float x.Minimum * float x.Minimum

    override x.Add (t, a) =
        if t < lastTime then
            failwithf "The current time cannot be less than the previous one."
        // elif Double.IsNaN (a) then
        //     x :> TimingStats<_>
        elif count = 0 then
            IntTimingStats (1, a, a, a, t, t, t, t, 0.0, 0.0) :> TimingStats<_>
        else
            let count = 1 + count
            let timeOfMinimum = if a < minimum then t else timeOfMinimum
            let timeOfMaximum = if a > maximum then t else timeOfMaximum
            let minimum = min a minimum
            let maximum = max a maximum
            let sum = sum + float last * (t - lastTime)
            let sum2 = sum2 + float (last * last) * (t - lastTime) 
            let lastTime = t
            IntTimingStats (count, minimum, maximum, a, timeOfMinimum, timeOfMaximum,
                startTime, lastTime, sum, sum2) :> TimingStats<_>

    member x.ToDoubleStats () =
        DoubleTimingStats (count, float minimum, float maximum, float last, timeOfMinimum, timeOfMaximum,
                           startTime, lastTime, sum, sum2) :> TimingStats<_>

    override x.Normalise (n) =
        IntSamplingStats (n, minimum, maximum, x.Mean, x.Mean2) :> SamplingStats<_>

and [<Sealed>] DoubleTimingStats (count, minimum, maximum, last, timeOfMinimum, timeOfMaximum,
                                  startTime, lastTime, sum, sum2) =

    inherit TimingStats<float> ()

    static let empty =
        DoubleTimingStats (0, infinity, -infinity, nan, infinity, -infinity,
            infinity, -infinity, nan, nan) 

    static member Empty = empty :> TimingStats<_>

    override x.Count = count

    override x.Minimum = minimum

    override x.Maximum = maximum

    override x.Last = last

    override x.MinimumTime = timeOfMinimum

    override x.MaximumTime = timeOfMaximum

    override x.StartTime = startTime

    override x.LastTime = lastTime

    override x.Sum = sum

    override x.Sum2 = sum2

    override x.Mean =
        if x.Count = 0 then 
            nan
        elif x.LastTime > x.StartTime then
            x.Sum / (x.LastTime - x.StartTime)
        else
            x.Minimum

    override x.Mean2 =
        if x.Count = 0 then 
            nan
        elif x.LastTime > x.StartTime then
            x.Sum2 / (x.LastTime - x.StartTime)
        else
            x.Minimum * x.Minimum

    override x.Add (t, a) =
        if t < lastTime then
            failwithf "The current time cannot be less than the previous one."
        elif Double.IsNaN (a) then
            x :> TimingStats<_>
        elif count = 0 then
            DoubleTimingStats (1, a, a, a, t, t, t, t, 0.0, 0.0) :> TimingStats<_>
        else
            let count = 1 + count
            let timeOfMinimum = if a < minimum then t else timeOfMinimum
            let timeOfMaximum = if a > maximum then t else timeOfMaximum
            let minimum = min a minimum
            let maximum = max a maximum
            let sum = sum + last * (t - lastTime)
            let sum2 = sum2 + (last * last) * (t - lastTime) 
            let lastTime = t
            DoubleTimingStats (count, minimum, maximum, a, timeOfMinimum, timeOfMaximum,
                startTime, lastTime, sum, sum2) :> TimingStats<_>

    override x.Normalise (n) =
        DoubleSamplingStats (n, minimum, maximum, x.Mean, x.Mean2) :> SamplingStats<_>

[<AutoOpen>]
module TimingStatsExtensions =

    type TimingStats<'a> with

        member x.Write (w: TextWriter, indent: int, provider: IFormatProvider) =

            let tab = String.replicate indent " "

            w.Write (tab)
            w.Write ("count = ")
            w.WriteLine (Convert.ToString (x.Count, provider))
        
            w.Write (tab)
            w.Write ("mean = ")
            w.WriteLine (Convert.ToString (x.Mean, provider))
        
            w.Write (tab)
            w.Write ("std = ")
            w.WriteLine (Convert.ToString (x.Deviation, provider))

            w.Write (tab)
            w.Write ("min = ")
            w.Write (Convert.ToString (box x.Minimum, provider))
            w.Write (" (t = ")
            w.Write (Convert.ToString (x.MinimumTime, provider))
            w.WriteLine (")")

            w.Write (tab)
            w.Write ("max = ")
            w.Write (Convert.ToString (box x.Maximum, provider))
            w.Write (" (t = ")
            w.Write (Convert.ToString (x.MaximumTime, provider))
            w.WriteLine (")")

            w.Write (tab)
            w.Write ("t in [")
            w.Write (Convert.ToString (x.StartTime, provider))
            w.Write ("; ")
            w.Write (Convert.ToString (x.LastTime, provider))
            w.WriteLine ("]")

        member x.Write (w: TextWriter, indent: int) =
            x.Write (w, indent, NumberFormatInfo.CurrentInfo)

        member x.Write (w: TextWriter) =
            x.Write (w, 0)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module TimingStats =

    [<CompiledName ("EmptyInt32s")>]
    let emptyInts = IntTimingStats.Empty

    [<CompiledName ("EmptyDoubles")>]
    let emptyFloats = DoubleTimingStats.Empty

    [<CompiledName ("Add")>]
    let add t a (x: TimingStats<_>) = x.Add (t, a)

    [<CompiledName ("Count")>]
    let count (x: TimingStats<_>) = x.Count

    [<CompiledName ("Minimum")>]
    let minimum (x: TimingStats<_>) = x.Minimum

    [<CompiledName ("Maximum")>]
    let maximum (x: TimingStats<_>) = x.Maximum

    [<CompiledName ("Last")>]
    let last (x: TimingStats<_>) = x.Last

    [<CompiledName ("MinimumTime")>]
    let minimumTime (x: TimingStats<_>) = x.MinimumTime

    [<CompiledName ("MaximumTime")>]
    let maximumTime (x: TimingStats<_>) = x.MaximumTime

    [<CompiledName ("StartTime")>]
    let startTime (x: TimingStats<_>) = x.StartTime

    [<CompiledName ("LastTime")>]
    let lastTime (x: TimingStats<_>) = x.LastTime

    [<CompiledName ("Sum")>]
    let sum (x: TimingStats<_>) = x.Sum

    [<CompiledName ("Sum2")>]
    let sum2 (x: TimingStats<_>) = x.Sum2

    [<CompiledName ("Mean")>]
    let mean (x: TimingStats<_>) = x.Mean

    [<CompiledName ("Mean2")>]
    let mean2 (x: TimingStats<_>) = x.Mean2

    [<CompiledName ("Variance")>]
    let variance (x: TimingStats<_>) = x.Variance

    [<CompiledName ("Deviation")>]
    let deviation (x: TimingStats<_>) = x.Deviation

    [<CompiledName ("FromInt32sToDoubles")>]
    let fromIntsToFloats (x: TimingStats<int>) = 
        match x with
        | :? IntTimingStats as z -> z.ToDoubleStats ()
        | _ -> failwithf "Expected to get the integer statistics"

    [<CompiledName ("Normalise")>]
    let normalise n (x: TimingStats<_>) = x.Normalise (n)