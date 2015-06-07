
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
open Simulation.Aivika.Experiments

type ExperimentSpecsProvider () as provider =

    let mutable title = "Experiment Specs"
    let mutable description = "It shows the experiment specs"
    let mutable width = 400

    member x.Title with get() = title and set v = title <- v
    member x.Description with get () = description and set v = description <- v
    member x.Width with get () = width and set v = width <- v

    interface IExperimentProvider<HtmlTextWriter> with
        member x.CreateRenderer (ctx) =
            { new IExperimentRenderer with
                member x.BeginRendering () = ()
                member x.Simulate (signals, results) = eventive {
                        return { new IDisposable with
                            member x.Dispose () = () } }
                member x.EndRendering () =

                    let exp = ctx.Experiment 
                    let specs = exp.Specs
                    let writer = ctx.Writer
                    let formatInfo = exp.FormatInfo 

                    writer.WriteFullBeginTag ("h3")
                    writer.WriteEncodedText (provider.Title)
                    writer.WriteEndTag ("h3")

                    if provider.Description <> "" then

                        writer.WriteFullBeginTag ("p")
                        writer.WriteEncodedText (provider.Description)
                        writer.WriteEndTag ("p")

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
                    writer.WriteEncodedText (provider.Title)
                    writer.WriteEndTag ("p")
                    writer.WriteEndTag ("td")
                    writer.WriteEndTag ("tr")
                    writer.WriteFullBeginTag ("tr")
                    writer.WriteFullBeginTag ("td")
                    writer.WriteEncodedText (formatInfo.StartTimeText)
                    writer.WriteEndTag ("td")
                    writer.WriteFullBeginTag ("td")
                    writer.WriteEncodedText (Convert.ToString (specs.StartTime, formatInfo))
                    writer.WriteEndTag ("td")
                    writer.WriteEndTag ("tr")
                    writer.WriteFullBeginTag ("tr")
                    writer.WriteFullBeginTag ("td")
                    writer.WriteEncodedText (formatInfo.StopTimeText)
                    writer.WriteEndTag ("td")
                    writer.WriteFullBeginTag ("td")
                    writer.WriteEncodedText (Convert.ToString (specs.StopTime, formatInfo))
                    writer.WriteEndTag ("td")
                    writer.WriteEndTag ("tr")
                    writer.WriteFullBeginTag ("tr")
                    writer.WriteFullBeginTag ("td")
                    writer.WriteEncodedText (formatInfo.DTText)
                    writer.WriteEndTag ("td")
                    writer.WriteFullBeginTag ("td")
                    writer.WriteEncodedText (Convert.ToString (specs.DT, formatInfo))
                    writer.WriteEndTag ("td")
                    writer.WriteEndTag ("tr")
                    writer.WriteFullBeginTag ("tr")
                    writer.WriteFullBeginTag ("td")
                    writer.WriteEncodedText (formatInfo.RunCount)
                    writer.WriteEndTag ("td")
                    writer.WriteFullBeginTag ("td")
                    writer.WriteEncodedText (Convert.ToString (exp.RunCount, formatInfo))
                    writer.WriteEndTag ("td")
                    writer.WriteEndTag ("tr")
                    writer.WriteFullBeginTag ("tr")
                    writer.WriteFullBeginTag ("td")
                    writer.WriteEncodedText (formatInfo.IntegMethod)
                    writer.WriteEndTag ("td")
                    writer.WriteFullBeginTag ("td")

                    match specs.Method with
                    | Euler -> writer.Write (formatInfo.EulerText)
                    | RungeKutta2 -> writer.Write (formatInfo.RungeKutta2Text)
                    | RungeKutta4 -> writer.Write (formatInfo.RungeKutta4Text)

                    writer.WriteEndTag ("td")
                    writer.WriteEndTag ("tr")
                    writer.WriteEndTag ("table")
                    writer.WriteEndTag ("p") }
