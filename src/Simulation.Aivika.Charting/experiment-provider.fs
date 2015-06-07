
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

namespace Simulation.Aivika.Charting.Web

open System
open System.Globalization
open System.Web.UI

open Simulation.Aivika
open Simulation.Aivika.Results
open Simulation.Aivika.Experiments
open Simulation.Aivika.Experiments.Web

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ExperimentProvider =

    [<CompiledName ("TimeSeries")>]
    let timeSeries (series: ResultTransform) =
        let p = TimeSeriesProvider ()
        p.Series <- series
        p :> IExperimentProvider<HtmlTextWriter>

    [<CompiledName ("XYChart")>]
    let xyChart (seriesX: ResultTransform) (seriesY: ResultTransform) =
        let p = XYChartProvider ()
        p.SeriesX <- seriesX
        p.SeriesY <- seriesY
        p :> IExperimentProvider<HtmlTextWriter>

    [<CompiledName ("DeviationChart")>]
    let deviationChart (series: ResultTransform) =
        let p = DeviationChartProvider ()
        p.Series <- series
        p :> IExperimentProvider<HtmlTextWriter>

    [<CompiledName ("LastValueHistogram")>]
    let lastValueHistogram (series: ResultTransform) =
        let p = LastValueHistogramProvider ()
        p.Series <- series
        p :> IExperimentProvider<HtmlTextWriter>

    [<CompiledName ("Queue")>]
    let queue (series: ResultTransform) =
        let series1 = series >> ResultSet.findById QueueCountStatsId
        let series2 = series >> ResultSet.findById QueueWaitTimeId
        let series3 = series >> ResultSet.findById QueueLostCountId
        let series' = ResultTransform.concat [series; series1; series2; series3]
        [ExperimentProvider.description series';
         deviationChart series1;
         ExperimentProvider.lastValueStats series1;
         deviationChart series2;
         ExperimentProvider.lastValueStats series2;
         deviationChart series3;
         ExperimentProvider.lastValueStats series3;
         lastValueHistogram series3]
            |> ExperimentProvider.concat

    [<CompiledName ("InfiniteQueue")>]
    let infiniteQueue (series: ResultTransform) =
        let series1 = series >> ResultSet.findById QueueCountStatsId
        let series2 = series >> ResultSet.findById QueueWaitTimeId
        let series3 = series >> ResultSet.findById QueueRateId
        let series' = ResultTransform.concat [series; series1; series2; series3]
        [ExperimentProvider.description series';
         deviationChart series1;
         ExperimentProvider.lastValueStats series1;
         deviationChart series2;
         ExperimentProvider.lastValueStats series2;
         deviationChart series3;
         ExperimentProvider.lastValueStats series3]
            |> ExperimentProvider.concat

    [<CompiledName ("Server")>]
    let server (series: ResultTransform) =
        let series1 = series >> ResultSet.findById ServerProcessingFactorId
        let series2 = series >> ResultSet.findById ServerPreemptionFactorId
        let series' = ResultTransform.concat [series; series1; series2]
        [ExperimentProvider.description series';
         deviationChart series1;
         ExperimentProvider.lastValueStats series1;
         lastValueHistogram series1;
         deviationChart series2;
         ExperimentProvider.lastValueStats series2;
         lastValueHistogram series2]
            |> ExperimentProvider.concat

    [<CompiledName ("ArrivalTimer")>]
    let arrivalTimer (series: ResultTransform) =
        let series1 = series >> ResultSet.findById ArrivalProcessingTimeId
        let series' = ResultTransform.concat [series; series1]
        [ExperimentProvider.description series';
         deviationChart series1;
         ExperimentProvider.lastValueStats series1]
            |> ExperimentProvider.concat
