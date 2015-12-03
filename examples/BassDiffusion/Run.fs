
namespace Simulation.Aivika.Examples

open System
open System.Web.UI

open Simulation.Aivika
open Simulation.Aivika.Results

module Terminal =

    [<EntryPoint>]
    let main args =

        ResultSet.printInIntegTimes Model.specs Model.model

        Console.WriteLine()
        Console.WriteLine("Press Enter...")
        Console.ReadLine () |> ignore

        0
