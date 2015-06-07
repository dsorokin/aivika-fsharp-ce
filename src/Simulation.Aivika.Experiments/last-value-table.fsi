
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

/// It saves the final simulation results in the CSV file.
type LastValueTableProvider =

    interface IExperimentProvider<HtmlTextWriter>

    /// Initializes a new instance.
    new: unit -> LastValueTableProvider 

    /// Gets the title.
    member Title: string

    /// Sets the title.
    member Title: string with set

    /// Gets the description.
    member Description: string

    /// Sets the description.
    member Description: string with set

    /// Gets the text used when rendering a link to the CSV file. 
    member LinkText: string

    /// Sets the text used when rendering a link to the CSV file. 
    member LinkText: string with set

    /// Specifies the CSV file name (supports variable $TITLE).
    member File: ExperimentFilePath

    /// Specifies the CSV file name (supports variable $TITLE).
    member File: ExperimentFilePath with set

    /// Gets a separator that delimits cells in the row of the CSV file.
    member Separator: string

    /// Sets a separator that delimits cells in the row of the CSV file.
    member Separator: string with set

    /// Specifies how the results must be transformed before rendering.
    member Transform: ResultTransform

    /// Specifies how the results must be transformed before rendering.
    member Transform: ResultTransform with set

    /// Defines the series which should be saved in the CSV file.
    member Series: ResultTransform 

    /// Defines the series which should be saved in the CSV file.
    member Series: ResultTransform with set 

    /// Allows filtering rows when saving them in the CSV file.
    member Filter: Eventive<bool>

    /// Allows filtering rows when saving them in the CSV file.
    member Filter: Eventive<bool> with set
