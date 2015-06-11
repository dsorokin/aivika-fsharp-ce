
(* Example: Simulation of a Bouncing Ball.

   It is described in MATLAB & Simulink example available by the following link:

   http://www.mathworks.com/help/simulink/examples/simulation-of-a-bouncing-ball.html *)

#nowarn "40"

namespace Simulation.Aivika.Examples

open Simulation.Aivika
open Simulation.Aivika.SD
open Simulation.Aivika.Results

module Model =

    /// the simulation specs.
    let specs = 
        { StartTime = 0.0;
          StopTime = 25.0;
          DT = 0.01;
          Method = RungeKutta4;
          GeneratorType = StrongGenerator }

    /// the acceleration due to gravity
    let g = 9.81

    /// the initial velocity
    let v0 = 15.0

    /// the initial position
    let x0 = 10.0

    /// the coefficient of restitution of the ball
    let k = 0.8

    /// the simulation model
    let model: Simulation<ResultSet> = simulation {
        let rec v = integChoice (lazy dv) (num v0)
        and x  = integChoice (lazy dx) (num x0)
        and dv = 
            dynamics {
                let! x' = x
                let! v' = v
                if x' < 0.0
                    then return Choice1Of2 (- k * v')
                    else return Choice2Of2 (- g)
            }
        and dx = 
            dynamics {
                let! x' = x
                let! v' = v
                if x' < 0.0
                    then return Choice1Of2 0.0
                    else return Choice2Of2 v'
        }
        return
            [ResultSource.From("x", x, "position");
             ResultSource.From("v", v, "velocity")]
                |> ResultSet.create
    }
