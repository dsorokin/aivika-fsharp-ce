
(* Example: Work Stations in Series

   This is a model of two work stations connected in a series and separated by finite queues.
   It is described in different sources [1, 2]. So, this is chapter 7 of [2] and section 5.14 of [1].

   [1] A. Alan B. Pritsker, Simulation with Visual SLAM and AweSim, 2nd ed.
   [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006 

The maintenance facility of a large manufacturer performs two operations. 
These operations must be performed in series; operation 2 always follows operation 1. 
The units that are maintained are bulky, and space is available for only eight units 
including the units being worked on. A proposed design leaves space for two units 
between the work stations, and space for four units before work station 1. [..] 
Current company policy is to subcontract the maintenance of a unit if it cannot 
gain access to the in-house facility.

Historical data indicates that the time interval between requests for maintenance 
is exponentially distributed with a mean of 0.4 time units. Service times are also 
exponentially distributed with the first station requiring on the average 0.25 time 
units and the second station, 0.5 time units. Units are transported automatically 
from work station 1 to work station 2 in a negligible amount of time. If the queue of 
work station 2 is full, that is, if there are two units awaiting for work station 2, 
the first station is blocked and a unit cannot leave the station. A blocked work 
station cannot server other units. *)

#I "../../bin"
#r "../../bin/Simulation.Aivika.dll"
#r "../../bin/Simulation.Aivika.Results.dll"

open Simulation.Aivika
open Simulation.Aivika.Queues
open Simulation.Aivika.Results

/// the simulation specs
let specs = {

    StartTime=0.0; StopTime=300.0; DT=0.1; 
    Method=RungeKutta4; GeneratorType=StrongGenerator
}

/// the mean delay of the input arrivals distributed exponentially
let meanOrderDelay = 0.4 

/// the capacity of the queue before the first work places
let queueMaxCount1 = 4

/// the capacity of the queue before the second work places
let queueMaxCount2 = 2

/// the mean processing time distributed exponentially in 
/// the first work stations
let meanProcessingTime1 = 0.25

/// the mean processing time distributed exponentially in
/// the second work stations
let meanProcessingTime2 = 0.5

/// the simulation model
let model = simulation {

    // it will gather the statistics about the processing time
    let! arrivalTimer = ArrivalTimer.create

    // define a stream of input events
    let inputStream = Stream.randomExponential meanOrderDelay

    // create a queue in front of the first work stations
    let! queue1 =
        Queue.createUsingFCFS queueMaxCount1
            |> Eventive.runInStartTime

    // create a queue between the first and second work stations
    let! queue2 =
        Queue.createUsingFCFS queueMaxCount2
            |> Eventive.runInStartTime

    // create the first work station (server)
    let! workStation1 = 
        Server.createRandomExponential meanProcessingTime1

    // create the second work station (server)
    let! workStation2 = 
        Server.createRandomExponential meanProcessingTime2

    // the entire processor from input to output
    let entireProcessor =
        Queue.processorWithLost queue1 >>
        Server.processor workStation1 >>
        Queue.processor queue2 >>
        Server.processor workStation2 >>
        ArrivalTimer.processor arrivalTimer

    // start simulating the model
    do! inputStream
            |> entireProcessor
            |> Stream.sink
            |> Proc.runInStartTime

    // return the simulation results
    return [ResultSource.From ("queue1", queue1, 
                "Queue no. 1");
            ResultSource.From ("workStation1", workStation1, 
                "Work Station no. 1");
            ResultSource.From ("queue2", queue2, 
                "Queue no. 2");
            ResultSource.From ("workStation2", workStation2, 
                "Work Station no. 2");
            ResultSource.From ("arrivalTimer", arrivalTimer, 
                "The arrival timer")]
                    |> ResultSet.create
}

/// the model summary
let modelSummary =
    model |> Simulation.map ResultSet.summary
