
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
open System.Web.UI

open Simulation.Aivika
open Simulation.Aivika.Results
open Simulation.Aivika.Experiments

type LastValueProvider () as provider =

    let mutable title = "The Last Values"
    let mutable runTitle = "$TITLE / Run $RUN_INDEX of $RUN_COUNT"
    let mutable description = "It shows the values in the final time point(s)."
    let mutable transform: ResultTransform = id
    let mutable series: ResultTransform = id 

    member x.Title with get () = title and set v = title <- v
    member x.RunTitle with get () = runTitle and set v = runTitle <- v
    member x.Description with get () = description and set v = description <- v
    member x.Transform with get () = transform and set v = transform <- v
    member x.Series with get () = series and set v = series <- v

    interface IExperimentProvider<HtmlTextWriter> with
        member x.CreateRenderer (ctx) =

            let exp = ctx.Experiment 
            let writer = ctx.Writer
            let formatInfo = exp.FormatInfo
            let runCount = exp.RunCount

            let dict =
                [ for i = 1 to runCount do
                    yield (i - 1, ref []) ] |> Map.ofList

            { new IExperimentRenderer with
                member x.BeginRendering () = ()
                member x.Simulate (signals, results) = 
                    eventive {

                        let results = results |> provider.Transform 
                        let results = results |> provider.Series

                        let values = results |> ResultSet.formatStringValues formatInfo |> Seq.toArray

                        let h t = eventive {

                            let! runIndex = Parameter.runIndex |> Parameter.lift
                            let! pairs = 
                                [ for value in values do
                                    yield eventive {
                                        let  x = value.Name
                                        let! y = value.Data
                                        return (x, y)
                                    } ] |> Eventive.ofList

                            dict.[runIndex] := pairs 
                        }

                        return! signals.SignalInStopTime
                                    |> Signal.subscribe h
                    }
                member x.EndRendering () =

                    let renderPair x y =
                        writer.WriteFullBeginTag ("p")
                        writer.WriteEncodedText (x)
                        writer.WriteEncodedText (" = ")
                        writer.WriteEncodedText (y)
                        writer.WriteEndTag ("p")

                    let renderSingleRun () =
                        let pairs = !dict.[0]
                        for (x, y) in pairs do
                            renderPair x y

                    let renderMultipleRuns () =
                        for i = 1 to runCount do
                            let subtitle = provider.RunTitle
                            let subtitle = subtitle.Replace ("$RUN_INDEX", Convert.ToString (i, formatInfo))
                            let subtitle = subtitle.Replace ("$RUN_COUNT", Convert.ToString (runCount, formatInfo))
                            let subtitle = subtitle.Replace ("$TITLE", provider.Title)
                            writer.WriteFullBeginTag ("h4")
                            writer.WriteEncodedText (subtitle)
                            writer.WriteEndTag ("h4")
                            let pairs = !dict.[i - 1]
                            for (x, y) in pairs do
                                renderPair x y

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
