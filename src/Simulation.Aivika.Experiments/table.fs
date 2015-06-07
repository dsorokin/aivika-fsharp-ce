
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

type TableProvider () as provider =

    let mutable title = "Table"
    let mutable description = "This section contains the CSV file(s) with the simulation results."
    let mutable linkText = "Download the CSV file"
    let mutable runLinkText = "$LINK / Run $RUN_INDEX of $RUN_COUNT"
    let mutable file = UniqueFilePath "Table($RUN_INDEX).csv"
    let mutable separator = ""
    let mutable transform: ResultTransform = id
    let mutable series: ResultTransform = id 
    let mutable filter = eventive { return true }

    member x.Title with get () = title and set v = title <- v
    member x.Description with get () = description and set v = description <- v
    member x.LinkText with get () = linkText and set v = linkText <- v
    member x.RunLinkText with get () = runLinkText and set v = runLinkText <- v
    member x.File with get () = file and set v = file <- v
    member x.Separator with get () = separator and set v = separator <- v
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
                    let substitutions =
                        [("$RUN_INDEX", Convert.ToString (i, formatInfo));
                         ("$RUN_COUNT", Convert.ToString (runCount, formatInfo));
                         ("$TITLE", provider.Title)] 
                            |> Map.ofList
                    let filename = 
                        provider.File |> ExperimentFilePath.expand substitutions
                    let filename =
                        ctx.Resolve (ctx.Directory, filename)
                    yield (i - 1, filename) ] |> Map.ofList

            { new IExperimentRenderer with
                member x.BeginRendering () = ()
                member x.Simulate (signals, results) = 
                    eventive {

                        let separator = 
                            if provider.Separator <> "" then
                                provider.Separator
                            else
                                let numberInfo = (formatInfo :> IFormatProvider).GetFormat(typeof<NumberFormatInfo>)
                                match numberInfo with
                                | :? NumberFormatInfo as numberInfo ->
                                    if numberInfo.NumberDecimalSeparator = "," then ";" else ","
                                | _ -> ","

                        let results = results |> provider.Transform
                        let results = results |> provider.Series

                        let values = results |> ResultSet.formatStringValues formatInfo |> Seq.toArray

                        let! runIndex = Parameter.runIndex |> Parameter.lift
                        let filename  = dict.[runIndex]
                        let writer = new StreamWriter (filename)

                        for i = 0 to values.Length - 1 do
                            if i > 0 then writer.Write (separator)
                            writer.Write (values.[i].Name)
                        writer.WriteLine ()

                        let handle t = eventive {

                            for i = 0 to values.Length - 1 do
                                if i > 0 then writer.Write (separator)
                                let! x = values.[i].Data
                                writer.Write (x)
                            writer.WriteLine ()
                        }

                        let signal = ResultSet.signal results
                        let signal = ResultSignal.purify signals signal

                        let! holder =
                            signal
                                |> Signal.filterc (fun () -> provider.Filter) 
                                |> Signal.subscribe handle

                        return { new IDisposable with
                            member x.Dispose () =
                                holder.Dispose ()
                                writer.Dispose () 
                                if exp.Verbose then
                                    printfn "Generated file %s" filename }
                    }
                member x.EndRendering () =

                    let getLink filename =
                        Uri.EscapeUriString (Path.GetFileName (filename))

                    let renderLink filename text = 
                        writer.WriteFullBeginTag ("p")
                        writer.WriteBeginTag ("a")
                        writer.WriteAttribute ("href", getLink filename)
                        writer.Write (">")
                        writer.WriteEncodedText (text)
                        writer.WriteEndTag ("a")
                        writer.WriteEndTag ("p")

                    let renderSingleRun () =
                        let filename = dict.[0]
                        renderLink filename provider.LinkText

                    let renderMultipleRuns () =
                        for i = 1 to runCount do
                            let filename = dict.[i - 1]
                            let text = provider.RunLinkText
                            let text = text.Replace ("$RUN_INDEX", Convert.ToString (i, formatInfo))
                            let text = text.Replace ("$RUN_COUNT", Convert.ToString (runCount, formatInfo))
                            let text = text.Replace ("$LINK", provider.LinkText)
                            renderLink filename text

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
