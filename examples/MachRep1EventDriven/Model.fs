
(* This is a rewritten version of model MachRep1 based on
   the event-oriented paradigm of DES. *)

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

#nowarn "40"

namespace Simulation.Aivika.Examples

open Simulation.Aivika
open Simulation.Aivika.Queues
open Simulation.Aivika.Results

module Model =

    let specs = {

        StartTime=0.0; StopTime=10000.0; DT=0.05; 
        Method=RungeKutta4; GeneratorType=StrongGenerator
    }

    let meanUpTime = 1.0
    let meanRepairTime = 0.5

    let model = simulation {

        // total up time for all machines
        let totalUpTime = ref 0.0

        let rec machineBroken startUpTime =
            eventive {
        
                // the machine is broken

                let! finishUpTime = 
                    Dynamics.time |> Dynamics.lift

                totalUpTime := !totalUpTime 
                    + (finishUpTime - startUpTime)

                let! repairTime = 
                    Parameter.randomExponential meanRepairTime
                        |> Parameter.lift

                // register a new event

                do! machineRepaired
                        |> Eventive.enqueue 
                            (finishUpTime + repairTime)
            }
        and machineRepaired =
            eventive {
        
                // the machine is repaired

                let! startUpTime = 
                    Dynamics.time |> Dynamics.lift

                let! upTime = 
                    Parameter.randomExponential meanUpTime
                        |> Parameter.lift
            
                // register a new event

                do! machineBroken startUpTime
                        |> Eventive.enqueue 
                            (startUpTime + upTime) 
            }

        do! machineRepaired |> Eventive.runInStartTime
        do! machineRepaired |> Eventive.runInStartTime

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
