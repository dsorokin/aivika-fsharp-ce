
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

type DescriptionProvider () as provider =

    let mutable title = "Description"
    let mutable description = "It shows the information about the simulation entities:"
    let mutable transform: ResultTransform = id
    let mutable series: ResultTransform = id 

    member x.Title with get () = title and set v = title <- v
    member x.Description with get () = description and set v = description <- v
    member x.Transform with get () = transform and set v = transform <- v
    member x.Series with get () = series and set v = series <- v

    interface IExperimentProvider<HtmlTextWriter> with
        member x.CreateRenderer (ctx) =

            let exp = ctx.Experiment 
            let writer = ctx.Writer
            let formatInfo = exp.FormatInfo
            let runCount = exp.RunCount

            let names = ref [| |]
            let descrs = ref [| |]

            let lockobj = obj ()

            { new IExperimentRenderer with
                member x.BeginRendering () = ()
                member x.Simulate (signals, results) = 
                    eventive {

                        let results = results |> provider.Transform 
                        let results = results |> provider.Series

                        let sources = results |> ResultSet.sources |> Seq.toArray

                        let names'  = sources |> Array.map ResultSource.name
                        let descrs' = sources |> Array.map ResultSource.id |> Array.map (fun x -> formatInfo.ResultFormatInfo.GetDescription (x))

                        lock lockobj (fun () ->
                            if (!names).Length = 0 then
                                names := names'
                                descrs := descrs'
                            elif (!names).Length <> names'.Length then
                                failwithf "Series of different lengths are returned for different runs when displaying the description info.")

                        return { new IDisposable with
                                    member x.Dispose () = ()
                               }
                    }
                member x.EndRendering () =

                    let renderPair x y =
                        writer.WriteFullBeginTag ("p")
                        writer.WriteEncodedText (x)
                        writer.WriteEncodedText (" = ")
                        writer.WriteEncodedText (y)
                        writer.WriteEndTag ("p")

                    writer.WriteFullBeginTag ("h3")
                    writer.WriteEncodedText (provider.Title)
                    writer.WriteEndTag ("h3")

                    if provider.Description <> "" then

                        writer.WriteFullBeginTag ("p")
                        writer.WriteEncodedText (provider.Description)
                        writer.WriteEndTag ("p")

                    writer.WriteFullBeginTag ("ul")

                    for i = 0 to (!names).Length - 1 do

                        let name  = (!names).[i]
                        let descr = (!descrs).[i]

                        writer.WriteFullBeginTag ("li")
                        writer.WriteEncodedText (name)

                        match descr with
                        | None   -> ()
                        | Some x ->

                            writer.WriteEncodedText (" - ")
                            writer.WriteEncodedText (x)

                        writer.WriteEndTag ("li")

                    writer.WriteEndTag ("ul")

            }
