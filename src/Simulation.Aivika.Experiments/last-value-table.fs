
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

type LastValueTableProvider () as provider =

    let mutable title = "Last Value Table"
    let mutable description = "This section contains the CSV file with the simulation results in final time points."
    let mutable linkText = "Download the CSV file"
    let mutable file = UniqueFilePath "LastValueTable.csv"
    let mutable separator = ""
    let mutable transform: ResultTransform = id
    let mutable series: ResultTransform = id 
    let mutable filter = eventive { return true }

    member x.Title with get () = title and set v = title <- v
    member x.Description with get () = description and set v = description <- v
    member x.LinkText with get () = linkText and set v = linkText <- v
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
                    yield (i - 1, ref []) ] |> Map.ofList

            let names = ref [| |]

            let data  = 
                [| for i = 1 to runCount do
                    yield None |]

            let lockobj = obj ()

            { new IExperimentRenderer with
                member x.BeginRendering () = ()
                member x.Simulate (signals, results) = 
                    eventive {

                        let results = results |> provider.Transform 
                        let results = results |> provider.Series

                        let values = results |> ResultSet.formatStringValues formatInfo |> Seq.toArray

                        let names' = values |> Array.map (fun v -> v.Name)

                        lock lockobj (fun () ->
                            if (!names).Length = 0 then
                                names := names'
                            elif (!names).Length <> names'.Length then
                                failwithf "Series of different lengths are returned for different runs when rendering the CSV table.")

                        let h t = eventive {

                            let! runIndex = Parameter.runIndex |> Parameter.lift

                            let! ys =
                                [| for value in values do
                                        yield value.Data |]
                                    |> Eventive.ofArray

                            data.[runIndex] <- Some ys
                        }

                        return! signals.SignalInStopTime
                                    |> Signal.filterc (fun t -> provider.Filter)
                                    |> Signal.subscribe h
                    }
                member x.EndRendering () =

                    let separator = 
                        if provider.Separator <> "" then
                            provider.Separator
                        else
                            let numberInfo = (formatInfo :> IFormatProvider).GetFormat(typeof<NumberFormatInfo>)
                            match numberInfo with
                            | :? NumberFormatInfo as numberInfo ->
                                if numberInfo.NumberDecimalSeparator = "," then ";" else ","
                            | _ -> ","

                    let substitutions =
                        [("$TITLE", provider.Title)] 
                            |> Map.ofList

                    let filename = 
                        provider.File |> ExperimentFilePath.expand substitutions
                    let filename =
                        ctx.Resolve (ctx.Directory, filename)

                    use csv = new StreamWriter (filename)

                    csv.Write ("Run")
                    for i = 0 to (!names).Length - 1 do
                        csv.Write (separator)
                        csv.Write ((!names).[i])
                    csv.WriteLine ()

                    for j = 0 to runCount - 1 do
                        match data.[j] with
                        | None -> ()
                        | Some ys ->

                            csv.Write (j)
                            for i = 0 to ys.Length - 1 do
                                csv.Write (separator)
                                csv.Write (ys.[i])
                            csv.WriteLine ()

                    if exp.Verbose then
                        printfn "Generated file %s" filename

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

                    writer.WriteFullBeginTag ("h3")
                    writer.WriteEncodedText (provider.Title)
                    writer.WriteEndTag ("h3")

                    if provider.Description <> "" then

                        writer.WriteFullBeginTag ("p")
                        writer.WriteEncodedText (provider.Description)
                        writer.WriteEndTag ("p")

                    renderLink filename provider.LinkText
            }
