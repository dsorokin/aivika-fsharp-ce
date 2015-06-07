
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

namespace Simulation.Aivika.Experiments.Web

open System
open System.IO
open System.Web.UI
open System.Globalization

open Simulation.Aivika
open Simulation.Aivika.Results
open Simulation.Aivika.Experiments

type LastValueStatsProvider () as provider =

    let mutable title = "Last Value Statistics"
    let mutable description = "This section displays the statistics summary collected in final time points."
    let mutable width = 400
    let mutable transform: ResultTransform = id
    let mutable series: ResultTransform = id 
    let mutable filter = eventive { return true }

    member x.Title with get () = title and set v = title <- v
    member x.Description with get () = description and set v = description <- v
    member x.Width with get () = width and set v = width <- v
    member x.Transform with get () = transform and set v = transform <- v
    member x.Series with get () = series and set v = series <- v
    member x.Filter with get () = filter and set v = filter <- v

    interface IExperimentProvider<HtmlTextWriter> with
        member x.CreateRenderer (ctx) =

            let exp = ctx.Experiment 
            let writer = ctx.Writer
            let formatInfo = exp.FormatInfo
            let runCount = exp.RunCount

            let dict =
                [ for i = 1 to runCount do
                    yield (i - 1, ref []) ] |> Map.ofList

            let names = ref [| |]
            let stats = ref [| |]

            let lockobj = obj ()

            { new IExperimentRenderer with
                member x.BeginRendering () = ()
                member x.Simulate (signals, results) = 
                    eventive {

                        let results = results |> provider.Transform 
                        let results = results |> provider.Series

                        let values = results |> ResultSet.floatStatsChoiceValues |> Seq.toArray

                        let names' = values |> Array.map (fun v -> v.Name)
                        let data'  = values |> Array.map (fun v -> v.Data)

                        lock lockobj (fun () ->
                            if (!names).Length = 0 then
                                names := names'
                                stats := names' |> Array.map (fun v -> SamplingStats.emptyFloats)
                            elif (!names).Length <> names'.Length then
                                failwithf "Series of different lengths are returned for different runs when collecting statistics.")

                        let handle a =
                            eventive {
                                for i = 0 to data'.Length - 1 do
                                    let! st = data'.[i]
                                    lock lockobj (fun () ->
                                        (!stats).[i] <-
                                            (!stats).[i]
                                                |> SamplingStats.appendChoice st)
                            }

                        return! 
                            signals.SignalInStopTime
                                |> Signal.filterc (fun a -> provider.Filter)
                                |> Signal.subscribe handle 
                    }
                member x.EndRendering () =

                    writer.WriteFullBeginTag ("h3")
                    writer.WriteEncodedText (provider.Title)
                    writer.WriteEndTag ("h3")

                    if provider.Description <> "" then

                        writer.WriteFullBeginTag ("p")
                        writer.WriteEncodedText (provider.Description)
                        writer.WriteEndTag ("p")

                    let getDescription (id: ResultId) =
                        match formatInfo.ResultFormatInfo.GetDescription (id) with
                        | Some text -> text
                        | None -> ""

                    for i = 0 to (!names).Length - 1 do

                        let name  = (!names).[i]
                        let stats = (!stats).[i] 

                        writer.WriteFullBeginTag ("p")
                        writer.WriteBeginTag ("table")
                        writer.WriteAttribute ("frame", "border")
                        writer.WriteAttribute ("cellspacing", string 4)
                        writer.WriteAttribute ("width", string provider.Width)
                        writer.Write (">")
                        writer.WriteFullBeginTag ("tr")
                        writer.WriteBeginTag ("td")
                        writer.WriteAttribute ("colspan", "2")
                        writer.Write (">")
                        writer.WriteBeginTag ("p")
                        writer.WriteAttribute ("align", "center")
                        writer.Write (">")
                        writer.WriteEncodedText (name)
                        writer.WriteEndTag ("p")
                        writer.WriteEndTag ("td")
                        writer.WriteEndTag ("tr")
                        writer.WriteFullBeginTag ("tr")
                        writer.WriteFullBeginTag ("td")
                        writer.WriteEncodedText (getDescription SamplingStatsMeanId)
                        writer.WriteEndTag ("td")
                        writer.WriteFullBeginTag ("td")
                        writer.WriteEncodedText (Convert.ToString (SamplingStats.mean stats, formatInfo))
                        writer.WriteEndTag ("td")
                        writer.WriteEndTag ("tr")
                        writer.WriteFullBeginTag ("tr")
                        writer.WriteFullBeginTag ("td")
                        writer.WriteEncodedText (getDescription SamplingStatsDeviationId)
                        writer.WriteEndTag ("td")
                        writer.WriteFullBeginTag ("td")
                        writer.WriteEncodedText (Convert.ToString (SamplingStats.deviation stats, formatInfo))
                        writer.WriteEndTag ("td")
                        writer.WriteEndTag ("tr")
                        writer.WriteFullBeginTag ("tr")
                        writer.WriteFullBeginTag ("td")
                        writer.WriteEncodedText (getDescription SamplingStatsMinimumId)
                        writer.WriteEndTag ("td")
                        writer.WriteFullBeginTag ("td")
                        writer.WriteEncodedText (Convert.ToString (SamplingStats.minimum stats, formatInfo))
                        writer.WriteEndTag ("td")
                        writer.WriteEndTag ("tr")
                        writer.WriteFullBeginTag ("tr")
                        writer.WriteFullBeginTag ("td")
                        writer.WriteEncodedText (getDescription SamplingStatsMaximumId)
                        writer.WriteEndTag ("td")
                        writer.WriteFullBeginTag ("td")
                        writer.WriteEncodedText (Convert.ToString (SamplingStats.maximum stats, formatInfo))
                        writer.WriteEndTag ("td")
                        writer.WriteEndTag ("tr")
                        writer.WriteFullBeginTag ("tr")
                        writer.WriteFullBeginTag ("td")
                        writer.WriteEncodedText (getDescription SamplingStatsCountId)
                        writer.WriteEndTag ("td")
                        writer.WriteFullBeginTag ("td")
                        writer.WriteEncodedText (Convert.ToString (SamplingStats.count stats, formatInfo))
                        writer.WriteEndTag ("td")
                        writer.WriteEndTag ("tr")
                        writer.WriteEndTag ("table")
                        writer.WriteEndTag ("p")
            }
