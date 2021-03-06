
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
experiment.RunCount <- 1

let m  = ResultSet.findByName "M"
let c  = ResultSet.findByName "C"

let provider1 = TimeSeriesProvider ()
let provider2 = TimeSeriesProvider ()

provider1.Height <- 1000
provider2.Height <- 1000

provider1.Series <- m
provider2.Series <- c

let providers =
    [ExperimentProvider.experimentSpecs;
     provider1 :> IExperimentProvider<HtmlTextWriter>;
     provider2 :> IExperimentProvider<HtmlTextWriter>]

experiment.RenderHtml (Model.model 51, providers)
    |> Async.RunSynchronously
