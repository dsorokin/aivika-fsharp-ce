(* This is model Chemical Reaction from the 5-minute tutorial of Berkeley-Madonna *)

#nowarn "40"

namespace Simulation.Aivika.Examples

open System

open Simulation.Aivika
open Simulation.Aivika.Results
open Simulation.Aivika.SD

module Model =

    let specs = {

        StartTime=0.0; StopTime=13.0; DT=0.01; 
        Method=RungeKutta4; GeneratorType=StrongGenerator
    }

    let model : Simulation<ResultSet> = simulation {

        let rec a = integ (lazy (- ka * a)) (num 100.0)
        and b = integ (lazy (ka * a - kb * b)) (num 0.0)
        and c = integ (lazy (kb * b)) (num 0.0)
        and ka = 1.0
        and kb = 1.0

        return 
            [ResultSource.From ("A", a, "Var A");
             ResultSource.From ("B", b, "Var B");
             ResultSource.From ("C", c, "Var C")]
                |> ResultSet.create
    }
