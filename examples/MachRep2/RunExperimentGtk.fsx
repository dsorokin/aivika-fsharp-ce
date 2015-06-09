
#I "../../bin"
#r "../../bin/Simulation.Aivika.dll"
#r "../../bin/Simulation.Aivika.Results.dll"
#r "../../bin/Simulation.Aivika.Experiments.dll"
#r "../../bin/Simulation.Aivika.Charting.Gtk.dll"

#load "Model.fsx"

open System
open System.Web.UI

open Simulation.Aivika
open Simulation.Aivika.Results
open Simulation.Aivika.Experiments
open Simulation.Aivika.Experiments.Web
open Simulation.Aivika.Charting.Gtk.Web

let experiment = Experiment ()

experiment.Specs <- Model.specs
experiment.RunCount <- 1000

let provider1 = ExperimentSpecsProvider ()
let provider2 = DeviationChartProvider ()
let provider3 = LastValueStatsProvider ()
let provider4 = LastValueHistogramProvider ()

let providers =
    [ provider1 :> IExperimentProvider<HtmlTextWriter>;
      provider2 :> IExperimentProvider<HtmlTextWriter>;
      provider3 :> IExperimentProvider<HtmlTextWriter>;
      provider4 :> IExperimentProvider<HtmlTextWriter> ]

experiment.RenderHtml (Model.model, providers)
    |> Async.RunSynchronously
