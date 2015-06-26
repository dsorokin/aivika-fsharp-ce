
(* Example: Machine Tool with Breakdowns 

   It is described in different sources [1, 2]. So, this is chapter 13 of [2] and section 6.12 of [1].

  [1] A. Alan B. Pritsker, Simulation with Visual SLAM and AweSim, 2nd ed.
  [2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006
 
Jobs arrive to a machine tool on the average of one per hour. The distribution of these interarrival 
times is exponential. During normal operation, the jobs are processed on a first-in, first-out basis. 
The time to process a job in hours is normally distributed with a mean of 0.5 and a standard 
deviation of 0.1. In addition to the processing time, there is a set up time that is uniformly distributed 
between 0.2 and 0.5 of an hour. Jobs that have been processed by the machine tool are routed to a different 
section of the shop and are considered to have left the machine tool area.

The machine tool experiences breakdowns during which time it can no longer process jobs. The time 
between breakdowns is normally distributed with a mean of 20 hours and a standard deviation of 2 hours. 
When a breakdown occurs, the job being processed is removed from the machine tool and is placed at the head 
of the queue of jobs waiting to be processed. Jobs preempted restart from the point at which they were 
interrupted.

When the machine tool breaks down, a repair process is initiated which is accomplished in three phases. 
Each phase is exponentially distributed with a mean of 3/4 of an hour. Since the repair time is the sum of 
independent and identically distributed exponential random variables, the repair time is Erlang 
distributed. The machine tool is to be analyzed for 500 hours to obtain information on the utilization of 
the machine tool and the time required to process a job. Statistics are to be collected for thousand 
simulation runs. *)

#I "../../bin"
#r "../../bin/Simulation.Aivika.dll"
#r "../../bin/Simulation.Aivika.Results.dll"

open Simulation.Aivika
open Simulation.Aivika.Queues
open Simulation.Aivika.Results

/// the simulation specs
let specs = {

    StartTime=0.0; StopTime=500.0; DT=0.1; 
    Method=RungeKutta4; GeneratorType=StrongGenerator
}

/// How often do jobs arrive to a machine tool (exponential)?
let jobArrivingMu = 1.0

/// A mean of time to process a job (normal). 
let jobProcessingMu = 0.5

/// The standard deviation of time to process a job (normal).
let jobProcessingSigma = 0.1

/// The minimum set-up time (uniform).
let minSetUpTime = 0.2

/// The maximum set-up time (uniform).
let maxSetUpTime = 0.5

/// A mean of time between breakdowns (normal).
let breakdownMu = 20.0

/// The standard deviation of time between breakdowns (normal).
let breakdownSigma = 2.0

/// A mean of each of the three repair phases (Erlang).
let repairMu = 3.0 / 4.0

/// A priority of the job (less is higher)
let jobPriority = 1.0

/// A priority of the breakdown (less is higher)
let breakdownPriority = 0.0

/// The simulation model.
let model: Simulation<ResultSet> = simulation {
    // create an input queue
    let! inputQueue =
        InfiniteQueue.createUsingFCFS
            |> Eventive.runInStartTime
    // a counter of jobs completed
    let! jobsCompleted = ArrivalTimer.create
    // a counter of interrupted jobs
    let jobsInterrupted = ref 0
    // create an input stream
    let inputStream = Stream.randomExponential jobArrivingMu
    // create a preemptible resource
    let! tool = 
        PreemptibleResource.create 1
            |> Eventive.runInStartTime
    // the machine setting up
    let! machineSettingUp =
        Server.createRandomUniformPreemptible 
            true minSetUpTime maxSetUpTime
    // the machine processing
    let! machineProcessing =
        Server.createRandomNormalPreemptible
            true jobProcessingMu jobProcessingSigma
    // the machine breakdown
    let machineBreakdown = proc {
        while true do
            do! Proc.randomNormal_ breakdownMu breakdownSigma
            use! h = 
                PreemptibleResource.takeWithPriority 
                    breakdownPriority tool
            do! Proc.randomErlang_ repairMu 3
    }
    // start the process of breakdowns
    do! machineBreakdown 
            |> Proc.runInStartTime
    // update a counter of job interruptions
    do! machineProcessing
            |> Server.taskPreemptionBeginning 
            |> Signal.add (fun a -> eventive { incr jobsInterrupted })
            |> Eventive.runInStartTime
    // define the queue network
    let network = 
        InfiniteQueue.processor inputQueue >>
        Processor.within
            (PreemptibleResource.requestWithPriority jobPriority tool) >>
        Server.processor machineSettingUp >>
        Server.processor machineProcessing >>
        Processor.within
            (PreemptibleResource.release tool) >>
        ArrivalTimer.processor jobsCompleted
    // start the machine tool
    do! network inputStream
            |> Stream.sink
            |> Proc.runInStartTime
    // return the simulation results in start time
    return
        [ResultSource.From ("inputQueue", 
            inputQueue, "the queue of jobs");
         ResultSource.From ("machineSettingUp",
            machineSettingUp, "the machine setting up");
         ResultSource.From ("machineProcessing",
            machineProcessing, "the machine processing");
         ResultSource.From ("jobsInterrupted",
            jobsInterrupted, "a counter of the interrupted jobs");
         ResultSource.From ("jobsCompleted",
            jobsCompleted, "a counter of the completed jobs")]
                |> ResultSet.create
}

let modelSummary =
    model |> Simulation.map ResultSet.summary
