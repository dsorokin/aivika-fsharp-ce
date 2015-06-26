
(* It corresponds to model MachRep2 described in document 
   Introduction to Discrete-Event Simulation and the SimPy Language
   [http://heather.cs.ucdavis.edu/~matloff/156/PLN/DESimIntro.pdf]. 
   SimPy is available on [http://simpy.sourceforge.net/].
   
   The model description is as follows.
   
   Two machines, but sometimes break down. Up time is exponentially 
   distributed with mean 1.0, and repair time is exponentially distributed 
   with mean 0.5. In this example, there is only one repairperson, so 
   the two machines cannot be repaired simultaneously if they are down 
   at the same time.

   In addition to finding the long-run proportion of up time as in
   model MachRep1, let’s also find the long-run proportion of the time 
   that a given machine does not have immediate access to the repairperson 
   when the machine breaks down. Output values should be about 0.6 and 0.67. *)

namespace Simulation.Aivika.Examples

open System

open Simulation.Aivika
open Simulation.Aivika.Results

module Model =

    let specs = {

        StartTime=0.0; StopTime=1000.0; DT=1.0; 
        Method=RungeKutta4; GeneratorType=StrongGenerator
    }
    
    let meanUpTime = 1.0
    let meanRepairTime = 0.5

    let model = simulation {

        // number of times the machines have broken down
        let nRep = ref 0            

        // number of breakdonws in which the machine 
        // started repair service right away
        let nImmedRep = ref 0       

        // total up time for all machines
        let totalUpTime = ref 0.0   
    
        let! repairPerson = 
            Resource.createUsingFCFS 1
                |> Eventive.runInStartTime

        let machine = proc {
    
            while true do

                let! upTime = Proc.randomExponential meanUpTime
                totalUpTime := !totalUpTime + upTime 
            
                incr nRep
            
                let! n = 
                    Resource.count repairPerson
                        |> Eventive.lift
            
                if n = 1 then
                    incr nImmedRep
                
                do! Resource.request repairPerson
                let! repairTime = Proc.randomExponential meanRepairTime
                do! Resource.release repairPerson
        }
    
        do! Proc.runInStartTime machine
        do! Proc.runInStartTime machine
    
        let upTimeProp = eventive {
                
            let! t = Dynamics.time |> Dynamics.lift 
            return !totalUpTime / (2.0 * t)
        }
    
        let immedTimeProp = eventive {
            return (float !nImmedRep) / (float !nRep)
        }

        return [ResultSource.From ("upTimeProp", upTimeProp, 
                    "Long-run proportion of up time \
                    (must be about 0.6)");
                ResultSource.From ("immedTimeProp", immedTimeProp, 
                    "Long-run proportion of the time when \
                    immediate access to the repairperson \
                    (must be about 0.67)")]
                        |> ResultSet.create
    }
