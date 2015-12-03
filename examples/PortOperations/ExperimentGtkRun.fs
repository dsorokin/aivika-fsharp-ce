
namespace Simulation.Aivika.Examples

open System
open System.Web.UI

open Simulation.Aivika
open Simulation.Aivika.Results
open Simulation.Aivika.Experiments
open Simulation.Aivika.Experiments.Web
open Simulation.Aivika.Charting.Gtk.Web

module Experiment =

    [<EntryPoint>]
    let main args =

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

        Console.WriteLine()
        Console.WriteLine("Press Enter...")
        Console.ReadLine () |> ignore

        0
