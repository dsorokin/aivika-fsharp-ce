
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

namespace Simulation.Aivika.Charting.Web

open System
open System.Collections.Generic
open System.IO
open System.Web.UI
open System.Globalization
open System.Drawing
open System.Windows.Forms.DataVisualization.Charting

open Simulation.Aivika
open Simulation.Aivika.Results
open Simulation.Aivika.Experiments
open Simulation.Aivika.Charting

type LastValueHistogramProvider () as provider =

    let mutable title = "Histogram by Last Values"
    let mutable description = "It shows the histogram by values collected in the final time points."
    let mutable width = 640
    let mutable height = 480
    let mutable file = UniqueFilePath "LastValueHistogram"
    let mutable filter = eventive { return true }
    let mutable builder = Histogram.create BinningStrategy.sturges
    let mutable transform: ResultTransform = id
    let mutable series: ResultTransform = id 
    let mutable seriesColors = Array.copy Color.predefined

    member x.Title with get () = title and set v = title <- v
    member x.Description with get () = description and set v = description <- v
    member x.Width with get () = width and set v = width <- v
    member x.Height with get () = height and set v = height <- v
    member x.File with get () = file and set v = file <- v
    member x.Filter with get () = filter and set v = filter <- v
    member x.Builder with get () = builder and set v = builder <- v
    member x.Transform with get () = transform and set v = transform <- v
    member x.Series with get () = series and set v = series <- v
    member x.SeriesColors with get () = seriesColors and set v = seriesColors <- v 

    interface IExperimentProvider<HtmlTextWriter> with
        member x.CreateRenderer (ctx) =

            let exp = ctx.Experiment 
            let writer = ctx.Writer
            let formatInfo = exp.FormatInfo
            let runCount = exp.RunCount
            let specs = exp.Specs

            let filename = 
                provider.File 
                    |> ExperimentFilePath.changeExtension "png"
            let filename =
                ctx.Resolve (ctx.Directory, filename)

            let n = specs.IterationCount
            let times = specs.IntegTimes |> Seq.toArray

            assert (times.Length = n)

            let names = ref [| |]
            let stats = ref [| |]

            let lockobj = obj ()

            { new IExperimentRenderer with
                member x.BeginRendering () = ()
                member x.Simulate (signals, results) = 
                    eventive {

                        let results = results |> provider.Transform 
                        let results = results |> provider.Series

                        let values = results |> ResultSet.floatValues |> Seq.toArray

                        let names' = values |> Array.map (fun v -> v.Name)
                        let data'  = values |> Array.map (fun v -> v.Data)

                        lock lockobj (fun () ->
                            if (!names).Length = 0 then
                                names := names'
                                stats := names' |> Array.map (fun v -> List<_> ())
                            elif (!names).Length <> names'.Length then
                                failwithf "Series of different lengths are returned for different runs when plotting the histogram.")

                        let handle a =
                            eventive {
                                for i = 0 to data'.Length - 1 do
                                    let! st = data'.[i]
                                    lock lockobj (fun () ->
                                        (!stats).[i].Add (st))
                            }

                        return! 
                            signals.SignalInStopTime
                                |> Signal.filterc (fun a -> provider.Filter)
                                |> Signal.subscribe handle 
                    }
                member x.EndRendering () =

                    let hist =
                        (!stats) |> Seq.map (Seq.filter (fun a -> not (Double.IsNaN a || Double.IsInfinity a)))
                                 |> Seq.map (Seq.toArray)
                                 |> Seq.toArray
                                 |> provider.Builder

                    let barColor i =
                        let cs = provider.SeriesColors
                        cs.[i % cs.Length]

                    let bar i =

                        let name = (!names).[i]

                        let xs = hist |> Array.map (fun (k, bins) -> k)
                        let ys = hist |> Array.map (fun (k, bins) -> bins.[i])

                        let series = new Series ()
                                        
                        series.BorderColor <- barColor i
                        series.Color <- barColor i 
                        series.LegendText <- name
                        series.ChartType <- SeriesChartType.Column

                        series.Points.DataBindXY(xs, ys)
                        series

                    let chart = new Chart ()
                    chart.Width <- provider.Width
                    chart.Height <- provider.Height

                    let area = new ChartArea ()

                    chart.ChartAreas.Add (area)

                    let legend = new Legend ()
                    legend.Docking <- Docking.Bottom

                    chart.Legends.Add (legend)

                    for i = 0 to (!names).Length - 1 do
                        chart.Series.Add (bar i)

                    chart.SaveImage (filename, ChartImageFormat.Png)
                    chart.Dispose ()

                    if exp.Verbose then
                        printfn "Generated file %s" filename

                    let getLink filename =
                        Uri.EscapeUriString (Path.GetFileName (filename))

                    writer.WriteFullBeginTag ("h3")
                    writer.WriteEncodedText (provider.Title)
                    writer.WriteEndTag ("h3")

                    if provider.Description <> "" then

                        writer.WriteFullBeginTag ("p")
                        writer.WriteEncodedText (provider.Description)
                        writer.WriteEndTag ("p")

                    writer.WriteFullBeginTag ("p")
                    writer.WriteBeginTag ("image")
                    writer.WriteAttribute ("src", getLink filename)
                    writer.Write (">")
                    writer.WriteEndTag ("image")
                    writer.WriteEndTag ("p")
            }
