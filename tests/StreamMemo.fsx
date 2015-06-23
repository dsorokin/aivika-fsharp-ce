
(* A test of function Stream.memo *)

#I "../bin"
#r "../bin/Simulation.Aivika.dll"

open Simulation.Aivika

let specs = 
    { StartTime = 0.0;
      StopTime  = 10.0;
      DT = 0.1;
      Method = RungeKutta4;
      GeneratorType = StrongGenerator }

let model: Simulation<unit> =
    simulation {

        let display n a =
            proc {
                let! t = Dynamics.time |> Dynamics.lift
                printfn "n = %s, t = %f, a = %s" n t (string a)
            }

        let trace n (x: Stream<Arrival<_>>) =
            x |> Stream.consume (fun a -> display n a.Value)

        let s = Stream.randomUniform 1.0 2.0

        let memoizedS = Stream.memo s

        do! trace "MemoizedS   " memoizedS
                |> Proc.runInStartTime

        do! trace "MemoizedS(2)" memoizedS
                |> Proc.runInStartTime

        do! trace "MemoizedS(3)" memoizedS
                |> Proc.runInStartTime

        do! trace "MemoizedS(4)" memoizedS
                |> Proc.runInStartTime

        do! proc.Zero ()
                |> Proc.runInStopTime
    }

do model |> Simulation.run specs
