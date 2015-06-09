
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

let specs = Model.specs
let model = Model.model Model.randomParams

let experiment = Experiment ()

experiment.Specs <- specs
experiment.RunCount <- 1000

let income = 
    [ResultSet.findByName "netIncome";
     ResultSet.findByName "netCashFlow"]
        |> ResultTransform.concat
        
let cashFlow =
    [ResultSet.findByName "npvIncome";
     ResultSet.findByName "npvCashFlow"]
        |> ResultTransform.concat

let providers =
    [ExperimentProvider.experimentSpecs;
     ExperimentProvider.description income;
     ExperimentProvider.deviationChart income
     ExperimentProvider.lastValueStats income;
     ExperimentProvider.lastValueHistogram income;
     ExperimentProvider.description cashFlow;
     ExperimentProvider.deviationChart cashFlow;
     ExperimentProvider.lastValueStats cashFlow;
     ExperimentProvider.lastValueHistogram cashFlow]

experiment.RenderHtml (model, providers)
    |> Async.RunSynchronously
