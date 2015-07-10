
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
experiment.RunCount <- 100

let portTime = ResultSet.findByName "portTime"
let berth = ResultSet.findByName "berth"
let tug = ResultSet.findByName "tug"

let providers =
    [ExperimentProvider.experimentSpecs;
     ExperimentProvider.lastValueStats portTime;
     ExperimentProvider.resource berth;
     ExperimentProvider.resource tug]

experiment.RenderHtml (Model.model, providers)
    |> Async.RunSynchronously
