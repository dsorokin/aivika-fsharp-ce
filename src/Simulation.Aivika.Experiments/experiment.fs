
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

namespace Simulation.Aivika.Experiments

open System
open System.Globalization
open System.IO
open System.Web.UI

open Simulation.Aivika
open Simulation.Aivika.Results

type [<Class>] Experiment () =

    let mutable specs = 
        { StartTime = 0.0;
          StopTime  = 10.0;
          DT        = 0.01;
          Method    = RungeKutta4;
          GeneratorType = SimpleGenerator }

    let mutable transform = id
    let mutable runCount = 1
    let mutable directory = UniqueFilePath "experiment"
    let mutable title = "Simulation Experiment"
    let mutable description = ""
    let mutable verbose = true
    let mutable formatInfo = ExperimentFormatInfo.CurrentInfo

    member x.Specs with get () = specs and set v = specs <- v
    member x.Transform with get () = transform and set v = transform <- v 
    member x.RunCount with get () = runCount and set v = runCount <- v
    member x.Directory with get () = directory and set v = directory <- v
    member x.Title with get () = title and set v = title <- v
    member x.Description with get () = description and set v = description <- v
    member x.Verbose with get () = verbose and set v = verbose <- v
    member x.FormatInfo with get () = formatInfo and set v = formatInfo <- v

and [<Class>] ExperimentContext<'a> (exp: Experiment, dir: string, resolve: string -> ExperimentFilePath -> string, writer: 'a) =
                                     
    member x.Experiment = exp
    member x.Directory = dir
    member x.Writer = writer

    member x.Resolve (dir, path) = resolve dir path

and [<Interface>] IExperimentRenderer =

    abstract BeginRendering: unit -> unit
    abstract Simulate: signals:PredefinedSignalSet * results:ResultSet -> Eventive<IDisposable>
    abstract EndRendering: unit -> unit

and [<Interface>] IExperimentProvider<'a> =

    abstract CreateRenderer: ctx:ExperimentContext<'a> -> IExperimentRenderer

and ExperimentFilePath = 
    | WritableFilePath of string
    | UniqueFilePath of string

and [<AbstractClass>] ExperimentFormatInfo (provider: ResultFormatInfo) as info = 

    interface IFormatProvider with

        member x.GetFormat (tp) =
            if tp = typeof<Experiment> then box info 
            else (provider :> IFormatProvider).GetFormat (tp)

    abstract StartTimeText: string
    abstract StopTimeText: string
    abstract DTText: string
    abstract RunCount: string
    abstract IntegMethod: string
    abstract EulerText: string
    abstract RungeKutta2Text: string
    abstract RungeKutta4Text: string

    member x.ResultFormatInfo = provider

    static member CreateInfo (provider) =
        { new ExperimentFormatInfo (provider) with

            override x.StartTimeText = "start time"
            override x.StopTimeText = "stop time"
            override x.DTText = "time step"
            override x.RunCount = "run count"
            override x.IntegMethod = "integration method"
            override x.EulerText = "Euler's method"
            override x.RungeKutta2Text = "the 2-nd order Runge-Kutta"
            override x.RungeKutta4Text = "the 4-th order Runge-Kutta" }

    static member CurrentInfo = ExperimentFormatInfo.CreateInfo (ResultFormatInfo.CurrentInfo)
    static member InvariantInfo = ExperimentFormatInfo.CreateInfo (ResultFormatInfo.InvariantInfo)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ExperimentFilePath =

    [<CompiledName ("Map")>]
    let map f = function
        | WritableFilePath x -> WritableFilePath (f x)
        | UniqueFilePath x -> UniqueFilePath (f x)

    [<CompiledName ("Expand")>]
    let expand substitutions path =

        let expand0 (state: string) (k: string) (v: string) = state.Replace (k, v)
        let expand' x = Map.fold expand0 x substitutions

        match path with
        | WritableFilePath x -> WritableFilePath (expand' x)
        | UniqueFilePath x -> UniqueFilePath (expand' x)

    [<CompiledName ("ChangeExtension")>]
    let changeExtension ext = function
        | WritableFilePath x -> WritableFilePath (Path.ChangeExtension (x, ext))
        | UniqueFilePath x -> UniqueFilePath (Path.ChangeExtension (x, ext))

[<AutoOpen>]
module ExperimentExtensions =

    type Experiment with

        member x.Render (model: Simulation<ResultSet>, providers:IExperimentProvider<'a> list, ctx: ExperimentContext<'a>) =
            async {

                let renderers = providers |> List.map (fun p -> p.CreateRenderer (ctx))

                for renderer in renderers do
                    renderer.BeginRendering ()

                let specs = x.Specs
                let runCount = x.RunCount
                let runs = 
                    simulation {

                        let! signals = PredefinedSignalSet.create |> Eventive.runInStartTime
                        let! results = model |> Simulation.map x.Transform
                        let! holders =
                            [ for renderer in renderers do
                                yield renderer.Simulate (signals, results)
                                            |> Eventive.runWith EarlierEvents
                                            |> Dynamics.runInStartTime ] |> Simulation.ofList

                        try
                            try
                                do! eventive.Zero () 
                                        |> Eventive.runInStopTime

                            with 
                                | :? SimulationException as e ->
                                    if x.Verbose then
                                        failwithf "A simulation exception has been raised when running: %s" e.Message
                        finally
                            for holder in holders do
                                holder.Dispose ()

                    } |> Simulation.runSeries runCount specs

                let! units = Async.Parallel (runs)

                for renderer in renderers do
                    renderer.EndRendering ()
            }

        member x.RenderHtml (model: Simulation<ResultSet>, providers: IExperimentProvider<HtmlTextWriter> list) =
            async {

                let dictlock = obj ()
                let dict = ref Set.empty

                let resolve dir path =
                    match path with
                    | WritableFilePath path ->
                        Path.Combine (dir, path)
                    | UniqueFilePath path ->
                        let name = Path.GetFileNameWithoutExtension (path)
                        let ext  = Path.GetExtension (path)
                        let rec loop y i =
                            let n  = Path.Combine (dir, if ext.Length > 0 then Path.ChangeExtension (y, ext) else y)
                            let nextName () = name + "(" + string i + ")"
                            if File.Exists (n) || Directory.Exists (n) then 
                                loop (nextName ()) (i + 1)
                            else 
                                let n' = 
                                    lock dictlock <| fun () ->
                                        if Set.contains n !dict then
                                            None
                                        else
                                            dict := Set.add n !dict 
                                            Some n
                                match n' with
                                | None -> loop (nextName ()) (i + 1)
                                | Some n' -> n'
                        loop name 2

                let dir = resolve "" x.Directory

                if x.Verbose then
                    printfn "Updating directory %s" dir

                Directory.CreateDirectory (dir) |> ignore

                use writer = new HtmlTextWriter (new StreamWriter (Path.Combine (dir, "index.html")))
                let ctx = ExperimentContext (x, dir, resolve, writer)

                writer.WriteFullBeginTag ("html")
                writer.WriteFullBeginTag ("head")
                writer.WriteBeginTag ("meta")
                writer.WriteAttribute ("http-equiv", "Content-Type")
                writer.WriteAttribute ("content", "text/html; charset=utf-8")
                writer.Write (">")
                writer.WriteFullBeginTag ("title")
                writer.WriteEncodedText (x.Title)
                writer.WriteEndTag ("title")
                writer.WriteBeginTag ("style")
                writer.WriteAttribute ("type", "text/css")
                writer.Write (">")
                writer.WriteLine ()

                writer.WriteLine ("* { margin: 0; padding: 0 }")
                writer.WriteLine ()
                writer.WriteLine ("html {")
                writer.WriteLine ("  background-color: white;")
                writer.WriteLine ("  width: 100%;")
                writer.WriteLine ("  height: 100%;")
                writer.WriteLine ("}")
                writer.WriteLine ()
                writer.WriteLine ("body {")
                writer.WriteLine ("  background: white;")
                writer.WriteLine ("  color: black;")
                writer.WriteLine ("  text-align: left;")
                writer.WriteLine ("  min-height: 100%;")
                writer.WriteLine ("  position: relative;")
                writer.WriteLine ("}")
                writer.WriteLine ()
                writer.WriteLine ("p {")
                writer.WriteLine ("  margin: 0.8em 0;")
                writer.WriteLine ("}")
                writer.WriteLine ()
                writer.WriteLine ("ul, ol {")
                writer.WriteLine ("  margin: 0.8em 0 0.8em 2em;")
                writer.WriteLine ("}")
                writer.WriteLine ()
                writer.WriteLine ("dl {")
                writer.WriteLine ("  margin: 0.8em 0;")
                writer.WriteLine ("}")
                writer.WriteLine ()
                writer.WriteLine ("dt {")
                writer.WriteLine ("  font-weight: bold;")
                writer.WriteLine ("}")
                writer.WriteLine ()
                writer.WriteLine ("dd {")
                writer.WriteLine ("  margin-left: 2em;")
                writer.WriteLine ("}")
                writer.WriteLine ()
                writer.WriteLine ("a { text-decoration: none; }")
                writer.WriteLine ("a[href]:link { color: rgb(196,69,29); }")
                writer.WriteLine ("a[href]:visited { color: rgb(171,105,84); }")
                writer.WriteLine ("a[href]:hover { text-decoration:underline; }")
                writer.WriteLine ()
                writer.WriteLine ("body {")
                writer.WriteLine ("  font:13px/1.4 sans-serif;")
                writer.WriteLine ("  *font-size:small; /* for IE */")
                writer.WriteLine ("  *font:x-small; /* for IE in quirks mode */")
                writer.WriteLine ("}")
                writer.WriteLine ()
                writer.WriteLine ("h1 { font-size: 146.5%; /* 19pt */ } ")
                writer.WriteLine ("h2 { font-size: 131%;   /* 17pt */ }")
                writer.WriteLine ("h3 { font-size: 116%;   /* 15pt */ }")
                writer.WriteLine ("h4 { font-size: 100%;   /* 13pt */ }")
                writer.WriteLine ("h5 { font-size: 100%;   /* 13pt */ }")
                writer.WriteLine ()
                writer.WriteLine ("select, input, button, textarea {")
                writer.WriteLine ("  font:99% sans-serif;")
                writer.WriteLine ("}")
                writer.WriteLine ()
                writer.WriteLine ("table {")
                writer.WriteLine ("  font-size:inherit;")
                writer.WriteLine ("  font:100%;")
                writer.WriteLine ("}")
                writer.WriteLine ()
                writer.WriteLine ("pre, code, kbd, samp, tt, .src {")
                writer.WriteLine ("  font-family:monospace;")
                writer.WriteLine ("  *font-size:108%;")
                writer.WriteLine ("  line-height: 124%;")
                writer.WriteLine ("}")
                writer.WriteLine ()
                writer.WriteLine (".links, .link {")
                writer.WriteLine ("  font-size: 85%; /* 11pt */")
                writer.WriteLine ("}")
                writer.WriteLine ()
                writer.WriteLine (".info  {")
                writer.WriteLine ("  font-size: 85%; /* 11pt */")
                writer.WriteLine ("}")
                writer.WriteLine ()
                writer.WriteLine (".caption, h1, h2, h3, h4, h5, h6 { ")
                writer.WriteLine ("  font-weight: bold;")
                writer.WriteLine ("  color: rgb(78,98,114);")
                writer.WriteLine ("  margin: 0.8em 0 0.4em;")
                writer.WriteLine ("}")
                writer.WriteLine ()
                writer.WriteLine ("* + h1, * + h2, * + h3, * + h4, * + h5, * + h6 {")
                writer.WriteLine ("  margin-top: 2em;")
                writer.WriteLine ("}")
                writer.WriteLine ()
                writer.WriteLine ("h1 + h2, h2 + h3, h3 + h4, h4 + h5, h5 + h6 {")
                writer.WriteLine ("  margin-top: inherit;")
                writer.WriteLine ("}")
                writer.WriteLine ()
                writer.WriteLine ("ul.links {")
                writer.WriteLine ("  list-style: none;")
                writer.WriteLine ("  text-align: left;")
                writer.WriteLine ("  float: right;")
                writer.WriteLine ("  display: inline-table;")
                writer.WriteLine ("  margin: 0 0 0 1em;")
                writer.WriteLine ("}")
                writer.WriteLine ()
                writer.WriteLine ("ul.links li {")
                writer.WriteLine ("  display: inline;")
                writer.WriteLine ("  border-left: 1px solid #d5d5d5; ")
                writer.WriteLine ("  white-space: nowrap;")
                writer.WriteLine ("  padding: 0;")
                writer.WriteLine ("}")
                writer.WriteLine ()
                writer.WriteLine ("ul.links li a {")
                writer.WriteLine ("  padding: 0.2em 0.5em;")
                writer.WriteLine ("}")

                writer.WriteEndTag ("style")
                writer.WriteEndTag ("head")
                writer.WriteFullBeginTag ("body")
                writer.WriteFullBeginTag ("h1")
                writer.WriteEncodedText (x.Title)
                writer.WriteEndTag ("h1")

                do! x.Render (model, providers, ctx)

                writer.WriteBreak ()
                writer.WriteFullBeginTag ("p")
                writer.WriteBeginTag ("font")
                writer.WriteAttribute ("size", "-1")
                writer.Write (">")

                // TODO: add a link!
                writer.Write ("Automatically generated by Aivika Simulation Framework")

                writer.WriteEndTag ("font")
                writer.WriteEndTag ("p")
                writer.WriteEndTag ("body")
                writer.WriteEndTag ("html")

                writer.Close ()

                if x.Verbose then
                    printfn "Generated file index.html"
            }

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ExperimentRenderer =

    [<CompiledName ("Empty")>]
    let empty =
        { new IExperimentRenderer with

            member r.BeginRendering () = ()
            member r.Simulate (signals, results) = 
                eventive {
                    return 
                        { new IDisposable with
                            member x.Dispose () = () }
                }
            member r.EndRendering () = ()
        } 

    [<CompiledName ("Append")>]
    let append (r1: IExperimentRenderer) (r2: IExperimentRenderer) =
        { new IExperimentRenderer with

            member r.BeginRendering () =

                r1.BeginRendering ()
                r2.BeginRendering ()

            member r.Simulate (signals, results) = 
                eventive {

                    let! h1 = r1.Simulate (signals, results)
                    let! h2 = r2.Simulate (signals, results)

                    return 
                        { new IDisposable with
                            member x.Dispose () =
                                h1.Dispose ()
                                h2.Dispose () }
                }

            member r.EndRendering () =

                r1.EndRendering ()
                r2.EndRendering () 
        } 

    [<CompiledName ("Concat")>]
    let concat (rs: IExperimentRenderer list) = 
        rs |> List.fold append (empty: IExperimentRenderer)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ExperimentProvider =

    [<CompiledName ("Empty")>]
    let empty<'a> =
        { new IExperimentProvider<'a> with

            member p.CreateRenderer (ctx) =
                ExperimentRenderer.empty
        }

    [<CompiledName ("Append")>]
    let append (p1: IExperimentProvider<'a>) (p2: IExperimentProvider<'a>) =
        { new IExperimentProvider<'a> with

            member p.CreateRenderer (ctx) =

                let r1 = p1.CreateRenderer (ctx)
                let r2 = p2.CreateRenderer (ctx)

                ExperimentRenderer.append r1 r2
        }

    [<CompiledName ("Concat")>]
    let concat (ps: IExperimentProvider<'a> list) = 
        ps |> List.fold append (empty: IExperimentProvider<'a>)

