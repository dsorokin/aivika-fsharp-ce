
(* A test of function Proc.memo *)

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
                printfn "n = %s, t = %f, a = %f" n t a
            }

        let p = Proc.randomExponential 0.1

        let memoizedP = Proc.memo p

        do! proc.Bind (memoizedP, display "MemoizedP   ")
                |> Proc.runInStartTime

        do! proc.Bind (memoizedP, display "MemoizedP(2)")
                |> Proc.runInStartTime

        do! proc.Bind (memoizedP, display "MemoizedP(3)")
                |> Proc.runInStartTime

        do! proc.Bind (memoizedP, display "MemoizedP(4)")
                |> Proc.runInStartTime

        do! proc.Zero ()
                |> Proc.runInStopTime
    }

do model |> Simulation.run specs
