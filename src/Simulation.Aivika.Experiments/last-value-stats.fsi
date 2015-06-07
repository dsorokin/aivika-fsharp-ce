
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

/// It shows the statistics summary gathered in final simulation time points.
type LastValueStatsProvider =

    interface IExperimentProvider<HtmlTextWriter>

    /// Initializes a new instance.
    new: unit -> LastValueStatsProvider 

    /// Gets the title.
    member Title: string

    /// Sets the title.
    member Title: string with set

    /// Gets the description.
    member Description: string

    /// Sets the description.
    member Description: string with set

    /// Gets the width of the HTML table.
    member Width: int

    /// Sets the width of the HTML table.
    member Width: int with set

    /// Specifies how the results must be transformed before rendering.
    member Transform: ResultTransform

    /// Specifies how the results must be transformed before rendering.
    member Transform: ResultTransform with set

    /// Defines the series for which the statistics summary should be collected in final time points.
    member Series: ResultTransform 

    /// Defines the series for which the statistics summary should be collected in final time points.
    member Series: ResultTransform with set 

    /// Allows filtering simulation runs when collecting the statistics.
    member Filter: Eventive<bool>

    /// Allows filtering simulation runs when collecting the statistics.
    member Filter: Eventive<bool> with set
