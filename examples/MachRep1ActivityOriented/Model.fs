
(* This is a rewritten version of model MachRep1 based on
   the activity-oriented paradigm of DES. *)

(* It corresponds to model MachRep1 described in document 
   Introduction to Discrete-Event Simulation and the SimPy Language
   [http://heather.cs.ucdavis.edu/~matloff/156/PLN/DESimIntro.pdf]. 
   SimPy is available on [http://simpy.sourceforge.net/].
   
   The model description is as follows.

   Two machines, which sometimes break down.
   Up time is exponentially distributed with mean 1.0, and repair time is
   exponentially distributed with mean 0.5. There are two repairpersons,
   so the two machines can be repaired simultaneously if they are down
   at the same time.

   Output is long-run proportion of up time. Should get value of about
   0.66. *)

namespace Simulation.Aivika.Examples

open System

open Simulation.Aivika
open Simulation.Aivika.Results

module Model =

    let specs = {

        StartTime=0.0; StopTime=1000.0; DT=0.05; 
        Method=RungeKutta4; GeneratorType=StrongGenerator
    }

    let meanUpTime = 1.0
    let meanRepairTime = 0.5

    let model = simulation {

        // total up time for all machines
        let totalUpTime = ref 0.0

        let machine () =

            // a number of iterations when working
            let upNum = ref -1

            // a number of iterations when prepairing
            let repairNum = ref -1

            // the start up time
            let startUpTime = ref 0.0

            // wait for the break
            let untilBroken = eventive {
                decr upNum
            }

            // wait for the repair
            let untilRepaired = eventive {
                decr repairNum
            }

            // when the tool is broken 
            let broken = eventive {

                decr upNum

                // the machine is broken

                let! t = Dynamics.time |> Dynamics.lift
                let! dt = Parameter.dt |> Parameter.lift
                let! repairTime =
                    Parameter.randomExponential meanRepairTime
                        |> Parameter.lift

                totalUpTime := !totalUpTime 
                    + (t - !startUpTime)
                repairNum := int (repairTime / dt)
            }

            // when the tool is repaired
            let repaired = eventive {

                decr repairNum    
                
                // the machine is repaired

                let! t = Dynamics.time |> Dynamics.lift
                let! dt = Parameter.dt |> Parameter.lift
                let! upTime =
                    Parameter.randomExponential meanUpTime
                        |> Parameter.lift 

                startUpTime := t
                upNum := int (upTime / dt)
            }

            // return a simulation model of the machine
            eventive {

                if !upNum > 0 then
                    return! untilBroken
                elif !upNum = 0 then
                    return! broken
                elif !repairNum > 0 then
                    return! untilRepaired
                elif !repairNum = 0 then
                    return! repaired
                else
                    return! repaired
            }

        // create two machines

        let m1 = machine ()
        let m2 = machine ()

        // start the machines

        do! m1 |> Eventive.enqueueWithIntegTimes
               |> Eventive.runInStartTime

        do! m2 |> Eventive.enqueueWithIntegTimes
               |> Eventive.runInStartTime

        // return the result

        let upTimeProp = 
            eventive {
                let! t = Dynamics.time |> Dynamics.lift
                return (!totalUpTime / (2.0 * t))
            }

        return 
            [ResultSource.From ("upTimeProp", upTimeProp, 
                "Long-run proportion of up time \
                (must be about 0.66)")]
                    |> ResultSet.create
    }
