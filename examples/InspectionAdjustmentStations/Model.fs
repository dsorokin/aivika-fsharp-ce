
(* Example: Inspection and Adjustment Stations on a Production Line
 
   This is a model of the workflow with a loop. Also there are two infinite queues.

   It is described in different sources [1, 2]. So, this is chapter 8 of [2] and section 5.15 of [1].

  [1] A. Alan B. Pritsker, Simulation with Visual SLAM and AweSim, 2nd ed.
  [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006
 
Assembled television sets move through a series of testing stations in the final 
stage of their production. At the last of these stations, the vertical control 
setting on the TV sets is tested. If the setting is found to be functioning improperly, 
the offending set is routed to an adjustment station where the setting is adjusted. 
After adjustment, the television set is sent back to the last inspection station where 
the setting is again inspected. Television sets passing the final inspection phase, 
whether for the first time of after one or more routings through the adjustment station, 
are routed to a packing area.

The time between arrivals of television sets to the final inspection station is uniformly 
distributed between 3.5 and 7.5 minutes. Two inspectors work side-by-side at the final 
inspection station. The time required to inspect a set is uniformly distributed between 
6 and 12 minutes. On the average, 85 percent of the sets are routed to the adjustment 
station which is manned by a single worker. Adjustment of the vertical control setting 
requires between 20 and 40 minutes, uniformly distributed.

The inspection station and adjustor are to be simulated for 480 minutes to estimate 
the time to process television sets through the final production stage and to determine 
the utilization of the inspectors and the adjustors. *)

namespace Simulation.Aivika.Examples

open Simulation.Aivika
open Simulation.Aivika.Queues
open Simulation.Aivika.Results

module Model =

    /// the simulation specs
    let specs = {

        StartTime=0.0; StopTime=480.0; DT=0.1; 
        Method=RungeKutta4; GeneratorType=StrongGenerator
    }

    /// the minimum delay of arriving the next TV set
    let minArrivalDelay = 3.5

    /// the maximum delay of arriving the next TV set
    let maxArrivalDelay = 7.5

    /// the minimum time to inspect the TV set
    let minInspectionTime = 6.0

    /// the maximum time to inspect the TV set
    let maxInspectionTime = 12.0

    /// the probability of passing the inspection phase
    let inspectionPassingProb = 0.85

    /// how many are inspection stations?
    let inspectionStationCount = 2

    /// the minimum time to adjust an improper TV set
    let minAdjustmentTime = 20.0

    /// the maximum time to adjust an improper TV set
    let maxAdjustmentTime = 40.0

    /// how many are adjustment stations?
    let adjustmentStationCount = 1

    let model: Simulation<ResultSet> = simulation {
        // to count the arrived TV sets for inspecting and adjusting
        let! inputArrivalTimer = ArrivalTimer.create
        // it will gather the statistics of the processing time
        let! outputArrivalTimer = ArrivalTimer.create
        // define a stream of input events
        let inputStream =
            Stream.randomUniform minArrivalDelay maxArrivalDelay 
        // create a queue before the inspection stations
        let! inspectionQueue =
            InfiniteQueue.createUsingFCFS
                |> Eventive.runInStartTime
        // create a queue before the adjustment stations
        let! adjustmentQueue =
            InfiniteQueue.createUsingFCFS
                |> Eventive.runInStartTime
        // create the inspection stations (servers)
        let! inspectionStations =
            [ for i = 1 to inspectionStationCount do
                yield Server.createRandomUniform
                    minInspectionTime maxInspectionTime ]
                |> Simulation.ofList
        // create the adjustment stations (servers)
        let! adjustmentStations =
            [ for i = 1 to adjustmentStationCount do
                yield Server.createRandomUniform
                    minAdjustmentTime maxAdjustmentTime ]
                |> Simulation.ofList
        // the line of parallel inspection stations
        let inspectionProcessor =
            inspectionStations
                |> List.map Server.processor
                |> Processor.par
        // the line of adjustment stations
        let adjustmentProcessor =
            adjustmentStations
                |> List.map Server.processor
                |> Processor.par
        // an output stream that comes after the inspection stations
        let rec outputStream = stream {
                let xs: Stream<_> = 
                    inspectionQueue 
                        |> InfiniteQueue.dequeue
                        |> Stream.repeat
                        |> inspectionProcessor
                for a in xs do
                    let! passed =
                        Parameter.randomTrue inspectionPassingProb
                            |> Parameter.lift
                    if passed then
                        yield a
                    else
                        do! adjustmentQueue
                                |> InfiniteQueue.enqueue a
                                |> Eventive.lift
            }
        // the terminal processor
        and terminalProcessor =
            outputStream
                |> ArrivalTimer.processor outputArrivalTimer
        // the process of adjusting TV sets
        and adjustmentProcess = proc {
                let xs: Stream<_> =
                    adjustmentQueue
                        |> InfiniteQueue.dequeue
                        |> Stream.repeat
                        |> adjustmentProcessor
                for a in xs do
                    do! inspectionQueue
                            |> InfiniteQueue.enqueue a
                            |> Eventive.lift
            }
        // the input process
        and inputProcess = proc {
                let xs: Stream<_> =
                    inputStream
                        |> ArrivalTimer.processor inputArrivalTimer
                for a in xs do
                    do! inspectionQueue
                            |> InfiniteQueue.enqueue a
                            |> Eventive.lift
            }
        // run the process of adjustment
        do! adjustmentProcess
                |> Proc.runInStartTime
        // run the input process
        do! inputProcess
                |> Proc.runInStartTime
        // run the terminal processor
        do! terminalProcessor
                |> Stream.sink
                |> Proc.runInStartTime
        // return the simulation results
        return
            [ResultSource.From ("inspectionQueue", inspectionQueue, 
                "the inspection queue");
             ResultSource.From ("adjustmentQueue", adjustmentQueue, 
                "the adjustment queue");
             ResultSource.From ("inputArrivalTimer", inputArrivalTimer, 
                "the input arrival timer");
             ResultSource.From ("outputArrivalTimer", outputArrivalTimer, 
                "the output arrival timer");
             ResultSource.From ("inspectionStations", inspectionStations, 
                "the inspection stations");
             ResultSource.From ("adjustmentStations", adjustmentStations, 
                "the adjustment stations")]
                    |> ResultSet.create
    }

    let modelSummary: Simulation<ResultSet> =
        model |> Simulation.map ResultSet.summary 
