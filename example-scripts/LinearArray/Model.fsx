
(* Example: the model demonstrates the use of arrays
   as described in model Linear Array from Berkeley Madonna *)

#nowarn "40"

#I "../../bin"
#r "../../bin/Simulation.Aivika.dll"
#r "../../bin/Simulation.Aivika.Results.dll"

open Simulation.Aivika
open Simulation.Aivika.SD
open Simulation.Aivika.Results

let specs = 
    { StartTime = 0.0; 
      StopTime = 500.0; 
      DT = 0.1;
      Method = RungeKutta4;
      GeneratorType = StrongGenerator }

let model (n: int) : Simulation<ResultSet> = simulation {

    let rec m : Dynamics<float> array =
        [| for i = 1 to n do
            yield integ 
                (lazy (q 
                    + k * (c.[i - 1] - c.[i]) 
                    + k * (c.[i + 1] - c.[i])))
                        (num 0.0) |]
    and c : Dynamics<float> array =
        [| for i = 0 to n + 1 do
            if i = 0 || i = n + 1 then
                yield (num 0.0)
            else
                yield (m.[i - 1] / v) |]
    and q = 1.0
    and k = 2.0
    and v = 0.75

    return
        [ResultSource.From("M", m, "M");
         ResultSource.From("C", c, "C")]
            |> ResultSet.create
}
