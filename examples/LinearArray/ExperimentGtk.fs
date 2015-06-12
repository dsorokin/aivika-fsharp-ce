
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
        experiment.RunCount <- 1

        let m  = ResultSet.findByName "M"
        let c  = ResultSet.findByName "C"

        let provider1 = TimeSeriesProvider ()
        let provider2 = TimeSeriesProvider ()

        provider1.Series <- m
        provider2.Series <- c

        let providers =
            [ExperimentProvider.experimentSpecs;
             provider1 :> IExperimentProvider<HtmlTextWriter>;
             provider2 :> IExperimentProvider<HtmlTextWriter>]

        experiment.RenderHtml (Model.model 51, providers)
            |> Async.RunSynchronously

        Console.WriteLine()
        Console.WriteLine("Press Enter...")
        Console.ReadLine () |> ignore

        0
