
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

        experiment.RenderHtml (Model.model Model.randomParams, providers)
            |> Async.RunSynchronously

        Console.WriteLine()
        Console.WriteLine("Press Enter...")
        Console.ReadLine () |> ignore

        0
