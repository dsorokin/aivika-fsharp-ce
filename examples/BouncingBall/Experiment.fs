
namespace Simulation.Aivika.Examples

open System
open System.Web.UI

open Simulation.Aivika
open Simulation.Aivika.Results
open Simulation.Aivika.Experiments
open Simulation.Aivika.Experiments.Web
open Simulation.Aivika.Charting.Web

module Experiment =

    [<EntryPoint>]
    let main args =

        let experiment = Experiment ()

        experiment.Specs <- Model.specs
        experiment.RunCount <- 1

        let x  = ResultSet.findByName "x"
        let v  = ResultSet.findByName "v"
        let xv = ResultTransform.append x v

        let providers =
            [ExperimentProvider.experimentSpecs;
             ExperimentProvider.description xv;
             ExperimentProvider.timeSeries x;
             ExperimentProvider.timeSeries v]

        experiment.RenderHtml (Model.model, providers)
            |> Async.RunSynchronously

        Console.WriteLine()
        Console.WriteLine("Press Enter...")
        Console.ReadLine () |> ignore

        0
