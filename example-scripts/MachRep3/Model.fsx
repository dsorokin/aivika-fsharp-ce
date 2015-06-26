
(* It corresponds to model MachRep3 described in document 
   Introduction to Discrete-Event Simulation and the SimPy Language
   [http://heather.cs.ucdavis.edu/~matloff/156/PLN/DESimIntro.pdf]. 
   SimPy is available on [http://simpy.sourceforge.net/].
   
   The model description is as follows.

   Variation of models MachRep1, MachRep2. Two machines, but
   sometimes break down. Up time is exponentially distributed with mean
   1.0, and repair time is exponentially distributed with mean 0.5. In
   this example, there is only one repairperson, and she is not summoned
   until both machines are down. We find the proportion of up time. It
   should come out to about 0.45. *)

#I "../../bin"
#r "../../bin/Simulation.Aivika.dll"
#r "../../bin/Simulation.Aivika.Results.dll"

open Simulation.Aivika
open Simulation.Aivika.Results

let specs = {

    StartTime=0.0; StopTime=1000.0; DT=1.0; 
    Method=RungeKutta4; GeneratorType=StrongGenerator
}
    
let meanUpTime = 1.0
let meanRepairTime = 0.5

let model = simulation {

    // number of machines currently up
    let nUp = ref 0

    // total up time for all machines
    let totalUpTime = ref 0.0
    
    let! repairPerson = 
        Resource.createUsingFCFS 1
            |> Eventive.runInStartTime

    let machine pid' = proc {
    
        incr nUp
    
        while true do
        
            let! upTime = Proc.randomExponential meanUpTime
            totalUpTime := !totalUpTime + upTime
                
            decr nUp    
                
            if !nUp = 1 then
                do! Proc.passivate
            else
                let! n = 
                    Resource.count repairPerson
                        |> Eventive.lift
                
                if n = 1 then
                    do! Proc.reactivate pid'
                            |> Eventive.lift
            
            do! Resource.request repairPerson
            let! repairTime = Proc.randomExponential meanRepairTime

            incr nUp
            
            do! Resource.release repairPerson
    }
    
    let! pid1 = Proc.createId
    let! pid2 = Proc.createId
    
    do! Proc.runInStartTimeUsingId pid1 (machine pid2)
    do! Proc.runInStartTimeUsingId pid2 (machine pid1)
    
    let upTimeProp = eventive {
            let! t = Dynamics.time |> Dynamics.lift
            return (!totalUpTime / (2.0 * t))
    }

    return [ResultSource.From ("upTimeProp", upTimeProp, 
                "The proportion of up time \
                (must be about 0.45)")]
                    |> ResultSet.create
}
