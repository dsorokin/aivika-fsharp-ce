
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
open System.Web.UI

open Simulation.Aivika
open Simulation.Aivika.Results

/// It defines the simulation experiment with the specified rendering backend and its bound data.
type [<Class>] Experiment =

    /// Initializes a new instance.
    new: unit -> Experiment

    /// Gets the simulation specs for the experiment.
    member Specs: Specs

    /// Sets the simulation specs for the experiment.
    member Specs: Specs with set

    /// Specifies how the results must be transformed before rendering.
    member Transform: ResultTransform

    /// Specifies how the results must be transformed before rendering.
    member Transform: ResultTransform with set

    /// Specifies how many simulation runs should be launched.
    member RunCount: int

    /// Specifies how many simulation runs should be launched.
    member RunCount: int with set

    /// Gets a directory template in which the output results should be saved.
    member Directory: ExperimentFilePath

    /// Sets a directory template in which the output results should be saved.
    member Directory: ExperimentFilePath with set

    /// Gets the experiment title.
    member Title: string

    /// Sets the experiment title.
    member Title: string with set

    /// Gets the experiment description.
    member Description: string

    /// Sets the experiment description.
    member Description: string with set

    /// Specifies whether the process of rendering the results is verbose.
    member Verbose: bool

    /// Specifies whether the process of rendering the results is verbose.
    member Verbose: bool with set

    /// Gets the locale settings used for rendering the results.
    member FormatInfo: ExperimentFormatInfo

    /// Sets the locale settings used for rendering the results.
    member FormatInfo: ExperimentFormatInfo with set

/// Defines an experiment context.
and [<Class>] ExperimentContext<'a> =

    /// Initializes a new instance.
    new: exp:Experiment * dir:string * resolve:(string -> ExperimentFilePath -> string) * writer:'a -> ExperimentContext<'a>

    /// The simulation experiment.
    member Experiment: Experiment
      
    /// The system file directory which the results should be saved in.
    member Directory: string

    /// A writer provided by the context.
    member Writer: 'a

    /// Resolves the file path template relative to the specified directory.
    member Resolve: dir:string * path:ExperimentFilePath -> string

/// It renders the simulation results when running the experiment.  
and [<Interface>] IExperimentRenderer =

    /// Begins rendering.
    abstract BeginRendering: unit -> unit

    /// It is called when running the simulation.
    abstract Simulate: signals:PredefinedSignalSet * results:ResultSet -> Eventive<IDisposable>

    /// Ends rendering.
    abstract EndRendering: unit -> unit

/// It provides with the renderer of the simulation results.
and [<Interface>] IExperimentProvider<'a> =

    /// Creates a renderer by the specified experiment context.
    abstract CreateRenderer: ctx:ExperimentContext<'a> -> IExperimentRenderer

/// Specifies the file path, unique or writable, which can be appended with extension if required.
and ExperimentFilePath = 
    /// The file which is overwritten in case if it existed before.
    | WritableFilePath of string
    /// The file which is always unique, when an automatically generated suffix is added to the name in case of need.
    | UniqueFilePath of string

/// The format info for the experiment.
and [<AbstractClass>] ExperimentFormatInfo = 

    interface IFormatProvider

    /// Initializes a new instance.
    new: provider:ResultFormatInfo -> ExperimentFormatInfo

    /// Translated text "start time".
    abstract StartTimeText: string

    /// Translated text "stop time".
    abstract StopTimeText: string

    /// Translated text "time step".
    abstract DTText: string

    /// Translated text "run count".
    abstract RunCount: string

    /// Tranlated text "integration method".
    abstract IntegMethod: string

    /// Translated text "Euler's method".
    abstract EulerText: string

    /// Translated text "the 2-nd order Runge-Kutta".
    abstract RungeKutta2Text: string

    /// Translated text "the 4-th order Runge-Kutta".
    abstract RungeKutta4Text: string

    /// Returns the result format info.
    member ResultFormatInfo: ResultFormatInfo

    /// Returns the current format info.
    static member CurrentInfo: ExperimentFormatInfo

    /// Returns the invariant format info.
    static member InvariantInfo: ExperimentFormatInfo

/// This module contains functions for working with the experiment file paths.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ExperimentFilePath =

    /// Maps a function over the file path.
    [<CompiledName ("Map")>]
    val map: f:(string -> string) -> path:ExperimentFilePath -> ExperimentFilePath

    /// Expand the file path using the specified table of substitutions.
    [<CompiledName ("Expand")>]
    val expand: substitutions:Map<string, string> -> path:ExperimentFilePath -> ExperimentFilePath

    /// Changes the extension name.
    [<CompiledName ("ChangeExtension")>]
    val changeExtension: ext:string -> path:ExperimentFilePath -> ExperimentFilePath

/// Defines extension methods.
[<AutoOpen>]
module ExperimentExtensions =

    type Experiment with

        /// Renders the simulation experiment.
        member Render: model:Simulation<ResultSet> * providers:IExperimentProvider<'a> list * ctx:ExperimentContext<'a> -> Async<unit>
        
        /// Renders the simulation experiment saving the results in the HTML page.
        member RenderHtml: model:Simulation<ResultSet> * providers:IExperimentProvider<HtmlTextWriter> list -> Async<unit>

/// This module contains functions for working with the experiment renderers.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ExperimentRenderer =

    /// It renders nothing.
    [<CompiledName ("Empty")>]
    val empty: IExperimentRenderer

    /// Appends two renderers.
    [<CompiledName ("Append")>]
    val append: r1:IExperimentRenderer -> r2:IExperimentRenderer -> IExperimentRenderer

    /// Concatenates the specified renderers.
    [<CompiledName ("Concat")>]
    val concat: rs:IExperimentRenderer list -> IExperimentRenderer

/// This module contains functions for working with the experiment providers.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ExperimentProvider =

    /// A provider that renders nothing.
    [<CompiledName ("Empty")>]
    val empty<'a> : IExperimentProvider<'a>

    /// Appends two providers.
    [<CompiledName ("Append")>]
    val append: p1:IExperimentProvider<'a> -> p2:IExperimentProvider<'a> -> IExperimentProvider<'a>

    /// Concatenates the specified providers.
    [<CompiledName ("Concat")>]
    val concat: ps:IExperimentProvider<'a> list -> IExperimentProvider<'a>

