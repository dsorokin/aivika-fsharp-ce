
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
open System.Web.UI

open OxyPlot

open Simulation.Aivika
open Simulation.Aivika.Results
open Simulation.Aivika.Experiments

/// It plots the histogram by last values and saves it in file.
type LastValueHistogramProvider =

    interface IExperimentProvider<HtmlTextWriter>

    /// Initializes a new instance.
    new: unit -> LastValueHistogramProvider 

    /// Gets the title.
    member Title: string

    /// Sets the title.
    member Title: string with set

    /// Gets the description.
    member Description: string

    /// Sets the description.
    member Description: string with set

    /// Gets the width of the histogram.
    member Width: int

    /// Sets the width of the histogram.
    member Width: int with set

    /// Gets the height of the histogram.
    member Height: int

    /// Sets the height of the histogram.
    member Height: int with set

    /// Specifies the file name for image.
    member File: ExperimentFilePath

    /// Specifies the file name for image.
    member File: ExperimentFilePath with set

    /// Allows filtering data when plotting them on the histogram.
    member Filter: Eventive<bool>

    /// Allows filtering data when plotting them on the histogram.
    member Filter: Eventive<bool> with set

    /// Gets the builder that creates a histogram by specified data.
    member Builder: (float array array -> Histogram)

    /// Sets the builder that creates a histogram by specified data.
    member Builder: (float array array -> Histogram) with set

    /// Specifies how the results must be transformed before collecting statistics.
    member Transform: ResultTransform

    /// Specifies how the results must be transformed before collecting statistics.
    member Transform: ResultTransform with set

    /// Gets the series that should be plotted on the histogram.
    member Series: ResultTransform 

    /// Sets the series that should be plotted on the histogram.
    member Series: ResultTransform with set 

    /// Gets the colors used for plotting the series on the histogram.
    member SeriesColors: OxyColor array

    /// Sets the colors used when plotting the series on the histogram.
    member SeriesColors: OxyColor array with set
