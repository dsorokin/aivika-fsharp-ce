
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

namespace Simulation.Aivika.Charting.Gtk.Web

open System
open System.Globalization
open System.Web.UI

open Simulation.Aivika
open Simulation.Aivika.Results
open Simulation.Aivika.Experiments

/// This module contains auxiliary functions for working with the experiment providers.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ExperimentProvider =

    /// Renders the time series for the specified series.
    [<CompiledName ("TimeSeries")>]
    val timeSeries: series:ResultTransform -> IExperimentProvider<HtmlTextWriter>

    /// Renders the XY Chart for the specified series.
    [<CompiledName ("XYChart")>]
    val xyChart: seriesX:ResultTransform -> seriesY:ResultTransform -> IExperimentProvider<HtmlTextWriter>

    /// Renders the deviation chart for the specified series.
    [<CompiledName ("DeviationChart")>]
    val deviationChart: series:ResultTransform -> IExperimentProvider<HtmlTextWriter>

    /// Renders the last value histogram for the specified series.
    [<CompiledName ("LastValueHistogram")>]
    val lastValueHistogram: series:ResultTransform -> IExperimentProvider<HtmlTextWriter>

    /// Renders the basic queue properties for the specified series.
    [<CompiledName ("Queue")>]
    val queue: series:ResultTransform -> IExperimentProvider<HtmlTextWriter>

    /// Renders the basic queue properties for the specified series.
    [<CompiledName ("InfiniteQueue")>]
    val infiniteQueue: series:ResultTransform -> IExperimentProvider<HtmlTextWriter>

    /// Renders the basic server properties for the specified series.
    [<CompiledName ("Server")>]
    val server: series:ResultTransform -> IExperimentProvider<HtmlTextWriter>

    /// Renders the basic arrival timer properties for the specified series.
    [<CompiledName ("ArrivalTimer")>]
    val arrivalTimer: series:ResultTransform -> IExperimentProvider<HtmlTextWriter>
