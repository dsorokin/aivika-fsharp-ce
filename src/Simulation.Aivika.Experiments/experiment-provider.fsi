
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
open System.Globalization
open System.Web.UI

open Simulation.Aivika
open Simulation.Aivika.Results
open Simulation.Aivika.Experiments

/// This module contains auxiliary functions for working with the experiment providers.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ExperimentProvider =

    /// Shows the experiment specs.
    [<CompiledName ("ExperimentSpecs")>]
    val experimentSpecs: IExperimentProvider<HtmlTextWriter>

    /// Shows the information about the specified series.
    [<CompiledName ("Description")>]
    val description: series:ResultTransform -> IExperimentProvider<HtmlTextWriter>

    /// Renders the last values for the specified series.
    [<CompiledName ("LastValue")>]
    val lastValue: series:ResultTransform -> IExperimentProvider<HtmlTextWriter>

    /// Renders the CSV file with results for the specified series.
    [<CompiledName ("Table")>]
    val table: series:ResultTransform -> IExperimentProvider<HtmlTextWriter>

    /// Renders the CSV file with last results for the specified series.
    [<CompiledName ("LastValueTable")>]
    val lastValueTable: series:ResultTransform -> IExperimentProvider<HtmlTextWriter>

    /// Renders the last value statistics for the specified series.
    [<CompiledName ("LastValueStats")>]
    val lastValueStats: series:ResultTransform -> IExperimentProvider<HtmlTextWriter>
