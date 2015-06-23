
(* A simple tests for statistics *)

#I "../bin"
#r "../bin/Simulation.Aivika.dll"
#r "../bin/Simulation.Aivika.Results.dll"

open Simulation.Aivika
open Simulation.Aivika.SD
open Simulation.Aivika.Results

let specs = 
    { StartTime = 0.0;
      StopTime  = 10.0;
      DT = 0.1;
      Method = RungeKutta4;
      GeneratorType = StrongGenerator }

let model: Simulation<ResultSet> =
    simulation {

        let x = Dynamics.memoRandomExponential (num 0.5)

        let s1 = ref SamplingStats.emptyFloats
        let s2 = ref TimingStats.emptyFloats

        do! eventive {
                let! t = Dynamics.time |> Dynamics.lift
                let! a = x |> Dynamics.lift
                s1 := !s1 |> SamplingStats.add a
                s2 := !s2 |> TimingStats.add t a
            } |> Eventive.enqueueWithIntegTimes
              |> Eventive.runInStartTime  

        return 
            [ResultSource.From ("s1", s1, "sampling statistics | E(s1) ~ 0.5");
             ResultSource.From ("s2", s2, "timing statistics | E(s2) ~ 0.5")]
                |> ResultSet.create 
    }

ResultSet.printInStopTime specs model
