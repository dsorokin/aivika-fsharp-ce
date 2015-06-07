
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
open System.IO
open System.Web.UI
open System.Globalization
open System.Windows.Forms.DataVisualization.Charting

open Simulation.Aivika
open Simulation.Aivika.Results
open Simulation.Aivika.Experiments
open Simulation.Aivika.Charting

type XYChartProvider () as provider =

    let mutable title = "XY Chart"
    let mutable runTitle = "$TITLE / Run $RUN_INDEX of $RUN_COUNT"
    let mutable description = "It shows the XY chart(s)."
    let mutable width = 640
    let mutable height = 480
    let mutable file = UniqueFilePath "XYChart($RUN_INDEX)"
    let mutable filter = eventive { return true }
    let mutable transform: ResultTransform = id
    let mutable seriesX: ResultTransform = ResultTransform.empty 
    let mutable seriesY: ResultTransform = id 
    let mutable seriesColors = Array.copy Color.predefined

    member x.Title with get () = title and set v = title <- v
    member x.RunTitle with get () = runTitle and set v = runTitle <- v
    member x.Description with get () = description and set v = description <- v
    member x.Width with get () = width and set v = width <- v
    member x.Height with get () = height and set v = height <- v
    member x.File with get () = file and set v = file <- v
    member x.Filter with get () = filter and set v = filter <- v
    member x.Transform with get () = transform and set v = transform <- v
    member x.SeriesX with get () = seriesX and set v = seriesX <- v
    member x.SeriesY with get () = seriesY and set v = seriesY <- v
    member x.SeriesColors with get () = seriesColors and set v = seriesColors <- v 

    interface IExperimentProvider<HtmlTextWriter> with
        member x.CreateRenderer (ctx) =

            let exp = ctx.Experiment 
            let writer = ctx.Writer
            let formatInfo = exp.FormatInfo
            let runCount = exp.RunCount

            let dict =
                [ for i = 1 to runCount do
                    let substitutions =
                        [("$RUN_INDEX", Convert.ToString (i, formatInfo));
                         ("$RUN_COUNT", Convert.ToString (runCount, formatInfo));
                         ("$TITLE", provider.Title)] 
                            |> Map.ofList
                    let filename = 
                        provider.File 
                            |> ExperimentFilePath.expand substitutions
                            |> ExperimentFilePath.changeExtension "png"
                    let filename =
                        ctx.Resolve (ctx.Directory, filename)
                    yield (i - 1, filename) ] |> Map.ofList

            { new IExperimentRenderer with
                member x.BeginRendering () = ()
                member x.Simulate (signals, results) = 
                    eventive {

                        let results = results |> provider.Transform
                         
                        let resultsX = results |> provider.SeriesX
                        let resultsY = results |> provider.SeriesY

                        let valuesX = resultsX |> ResultSet.floatValues |> Seq.toList
                        let valuesY = resultsY |> ResultSet.floatValues |> Seq.toList

                        let valuesX = 
                            match valuesX with
                            | [valuesX] -> valuesX
                            | _ -> failwithf "Expected a single X series for the XY Chart."

                        let! runIndex = Parameter.runIndex |> Parameter.lift
                        let filename  = dict.[runIndex]

                        let inputHistory (v: ResultValue<_>) =
                            let tr () =
                                eventive {
                                    let! x = valuesX.Data
                                    let! f = provider.Filter
                                    if f then
                                        let! y = v.Data 
                                        return (x, y)
                                    else
                                        return (x, nan)
                                }
                            v.Signal
                                |> ResultSignal.merge valuesX.Signal
                                |> ResultSignal.purify signals
                                |> Signal.mapc tr
                                |> SignalHistory.create

                        let! hsY = 
                            valuesY 
                                |> List.map inputHistory
                                |> Eventive.ofList

                        let finaliser = eventive {

                                let lineColor i =
                                    let cs = provider.SeriesColors
                                    cs.[i % cs.Length]

                                let line i (v: ResultValue<float>, h: SignalHistory<float * float>) = eventive {

                                        let! (ts, xys) = SignalHistory.read h

                                        let series = new Series()
                                        
                                        series.BorderColor <- lineColor i
                                        series.Color <- lineColor i 
                                        series.Name <- v.Name
                                        series.LegendText <- v.Name
                                        series.ChartType <- SeriesChartType.Line
                                        series.Points.DataBindXY(Array.map fst xys, Array.map snd xys)
                                        
                                        return series;
                                    }

                                let! linesY =
                                    List.zip valuesY hsY 
                                        |> List.mapi line
                                        |> Eventive.ofList

                                let chart = new Chart ()
                                chart.Width <- provider.Width
                                chart.Height <- provider.Height

                                let area = new ChartArea ()
                                area.AxisX.Title <- valuesX.Name

                                chart.ChartAreas.Add (area)

                                let legend = new Legend ()
                                legend.Docking <- Docking.Bottom

                                chart.Legends.Add(legend)

                                for line in linesY do
                                    chart.Series.Add (line)

                                chart.SaveImage (filename, ChartImageFormat.Png)
                                chart.Dispose ()

                                if exp.Verbose then
                                    printfn "Generated file %s" filename
                            }

                        return! Eventive.toDisposable finaliser
                    }
                member x.EndRendering () =

                    let getLink filename =
                        Uri.EscapeUriString (Path.GetFileName (filename))

                    let renderImage filename = 
                        writer.WriteFullBeginTag ("p")
                        writer.WriteBeginTag ("image")
                        writer.WriteAttribute ("src", getLink filename)
                        writer.Write (">")
                        writer.WriteEndTag ("image")
                        writer.WriteEndTag ("p")

                    let renderSingleRun () =
                        let filename = dict.[0]
                        renderImage filename

                    let renderMultipleRuns () =
                        for i = 1 to runCount do
                            let subtitle = provider.RunTitle
                            let subtitle = subtitle.Replace ("$RUN_INDEX", Convert.ToString (i, formatInfo))
                            let subtitle = subtitle.Replace ("$RUN_COUNT", Convert.ToString (runCount, formatInfo))
                            let subtitle = subtitle.Replace ("$TITLE", provider.Title)
                            writer.WriteFullBeginTag ("h4")
                            writer.WriteEncodedText (subtitle)
                            writer.WriteEndTag ("h4")
                            let filename = dict.[i - 1]
                            renderImage filename

                    writer.WriteFullBeginTag ("h3")
                    writer.WriteEncodedText (provider.Title)
                    writer.WriteEndTag ("h3")

                    if provider.Description <> "" then

                        writer.WriteFullBeginTag ("p")
                        writer.WriteEncodedText (provider.Description)
                        writer.WriteEndTag ("p")

                    if runCount = 1 then
                        renderSingleRun ()
                    else
                        renderMultipleRuns () 
            }
