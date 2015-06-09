
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

let inputQueue = ResultSet.findByName "inputQueue"

let machineSettingUp = ResultSet.findByName "machineSettingUp"
let machineProcessing = ResultSet.findByName "machineProcessing"

let jobsInterrupted = ResultSet.findByName "jobsInterrupted"
let jobsCompleted = ResultSet.findByName "jobsCompleted"

let providers =
    [ExperimentProvider.experimentSpecs;
     ExperimentProvider.infiniteQueue inputQueue;
     ExperimentProvider.server machineSettingUp;
     ExperimentProvider.server machineProcessing;
     ExperimentProvider.description jobsInterrupted;
     ExperimentProvider.lastValueStats jobsInterrupted;
     ExperimentProvider.arrivalTimer jobsCompleted]

experiment.RenderHtml (Model.model, providers)
    |> Async.RunSynchronously
