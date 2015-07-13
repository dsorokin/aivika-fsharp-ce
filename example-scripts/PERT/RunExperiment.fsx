
#I "../../bin"
#r "../../bin/Simulation.Aivika.dll"
#r "../../bin/Simulation.Aivika.Results.dll"
#r "../../bin/Simulation.Aivika.Experiments.dll"
#r "../../bin/Simulation.Aivika.Charting.dll"

#load "Model.fsx"

open System
open System.Web.UI

open Simulation.Aivika
open Simulation.Aivika.Results
open Simulation.Aivika.Experiments
open Simulation.Aivika.Experiments.Web
open Simulation.Aivika.Charting.Web

let experiment = Experiment ()

experiment.Specs <- Model.specs
experiment.RunCount <- 10000

let timer2 = ResultSet.findByName "timer2"
let timer3 = ResultSet.findByName "timer3"
let timer4 = ResultSet.findByName "timer4"
let timer5 = ResultSet.findByName "timer5"
let projCompletion = ResultSet.findByName "projCompletion"

let completionTime series =
    series |> ResultSet.findById ArrivalProcessingTimeId
           |> ResultSet.findById SamplingStatsMeanId 

let timers =
    [timer2; timer3; timer4; timer5;
     projCompletion]
        |> ResultTransform.concat

let lastValueStatsProvider =
    let p = new LastValueStatsProvider ()
    p.Title  <- "The Completion Time"
    p.Series <- timers
    p :> IExperimentProvider<HtmlTextWriter>

// let histogramProvider title series =
//    let p = new HistogramProvider ()
//    p.Title  <- title
//    p.Series <- completionTime series
//    p :> IExperimentProvider<HtmlTextWriter>

let providers =
    [ExperimentProvider.experimentSpecs;
     ExperimentProvider.description timers;
     lastValueStatsProvider]

experiment.RenderHtml (Model.model, providers)
    |> Async.RunSynchronously
