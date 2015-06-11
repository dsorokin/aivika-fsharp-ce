
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
experiment.RunCount <- 100

let series = 
    [ResultSet.findByName "potentialAdopters";
     ResultSet.findByName "adopters"]
        |> ResultTransform.concat

let providers =
    [ExperimentProvider.experimentSpecs;
     ExperimentProvider.description series;
     ExperimentProvider.deviationChart series]

experiment.RenderHtml (Model.model, providers)
    |> Async.RunSynchronously
