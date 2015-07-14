
(* Example: Analysis of a PERT-Type Network.

It is described in different sources [1, 2]. So, this is chapter 14 of [2] and section 7.11 of [1].

PERT is a technique for evaluating and reviewing a project consisting of
interdependent activities. A number of books have been written that describe
PERT modeling and analysis procedures. A PERT network activity descriptions
are given in a table stated below. All activity times will be assumed to be
triangularly distributed. For ease of description, activities have been
aggregated. The activities relate to power units, instrumentation, and
a new assembly and involve standard types of operations.

In the following description of the project, activity numbers are given
in parentheses. At the beginning of the project, three parallel activities
can be performed that involve: the disassembly of power units and
instrumentation (1); the installation of a new assembly (2); and
the preparation for a retrofit check (3). Cleaning, inspecting, and
repairing the power units (4) and calibrating the instrumentation (5)
can be done only after the power units and instrumentation have been
disassembled. Thus, activities 4 and 5 must follow activity 1 in the network.
Following the installation of the new assembly (2) and after the instrumentation
have been calibrated (5), a check of interfaces (6) and a check of
the new assembly (7) can be made. The retrofit check (9) can be made
after the assembly is checked (7) and the preparation for the retrofit
check (3) has been completed. The assembly and test of power units (8)
can be performed following the cleaning and maintenance of power units (4).
The project is considered completed when all nine activities are completed.
Since activities 6, 8, and 9 require the other activities to precede them,
their completion signifies the end of the project. This is indicated on
the network by having activities 6, 8, and 9 incident to node 6, the sink
node for the project. The objective of this example is to illustrate
the procedures for using Aivika to model and simulate project planning network.

Activity    Description                                  Mode Minimum Maximum Average

 1          Disassemble power units and instrumentation    3      1       5       3
 2          Install new assembly                           6      3       9       6
 3          Prepare for retrofit check                    13     10      19      14
 4          Clean, inspect, and repair power units         9      3      12       8
 5          Calibrate instrumentation                      3      1       8       4
 6          Check interfaces                               9      8      16      11
 7          Check assembly                                 7      4      13       8
 8          Assemble and test power units                  6      3       9       6
 9          Retrofit check                                 3      1       8       4

Node    Depends of Activities

 1              -
 2              1
 3              2, 5
 4              3, 7
 5              4
 6              6, 8, 9

Activity    Depends on Node

 1              1
 2              1
 3              1
 4              2
 5              2
 6              3
 7              3
 8              5
 9              4

[1] A. Alan B. Pritsker, Simulation with Visual SLAM and AweSim, 2nd ed.
[2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006

 *)

#nowarn "40"

namespace Simulation.Aivika.Examples

open Simulation.Aivika
open Simulation.Aivika.Queues
open Simulation.Aivika.Results

module Model =

    /// the simulation specs
    let specs = {

        StartTime=0.0; StopTime=1000.0; DT=0.1; 
        Method=RungeKutta4; GeneratorType=StrongGenerator
    }

    /// the simulation model
    let model = simulation {

        let! timer2 = ArrivalTimer.create
        let! timer3 = ArrivalTimer.create
        let! timer4 = ArrivalTimer.create
        let! timer5 = ArrivalTimer.create
        let! projCompletionTimer = ArrivalTimer.create

        let rec p1 = Processor.randomTriangular 1.0 3.0 5.0
        and p2 = Processor.randomTriangular 3.0 6.0 9.0
        and p3 = Processor.randomTriangular 10.0 13.0 19.0
        and p4 = Processor.randomTriangular 3.0 9.0 12.0
        and p5 = Processor.randomTriangular 1.0 3.0 8.0
        and p6 = Processor.randomTriangular 8.0 9.0 16.0
        and p7 = Processor.randomTriangular 4.0 7.0 13.0
        and p8 = Processor.randomTriangular 3.0 6.0 9.0
        and p9 = Processor.randomTriangular 1.0 3.0 8.0
        and c2 = ArrivalTimer.processor timer2
        and c3 = ArrivalTimer.processor timer3
        and c4 = ArrivalTimer.processor timer4
        and c5 = ArrivalTimer.processor timer5
        and c6 = ArrivalTimer.processor projCompletionTimer
        and i123 = Stream.clone 3 n1
        and i45 = Stream.clone 2 n2
        and i67 = Stream.clone 2 n3
        and i1 = List.nth i123 0
        and i2 = List.nth i123 1
        and i3 = List.nth i123 2
        and i4 = List.nth i45 0
        and i5 = List.nth i45 1
        and i6 = List.nth i67 0
        and i7 = List.nth i67 1
        and i8 = n5
        and i9 = n4
        and s1 = p1 i1
        and s2 = p2 i2
        and s3 = p3 i3
        and s4 = p4 i4
        and s5 = p5 i5
        and s6 = p6 i6
        and s7 = p7 i7
        and s8 = p8 i8
        and s9 = p9 i9
        and n1 = parameter.Return (0.0, 0.0)
                    |> Stream.randomArrival
                    |> Stream.take 1
        and n2 = s1 |> c2
        and n3 = s2 |> Stream.merge2 s5 
                    |> Stream.firstArrivals 2 
                    |> c3
        and n4 = s3 |> Stream.merge2 s7 
                    |> Stream.firstArrivals 2 
                    |> c4
        and n5 = s4 |> c5
        and n6 = s6 |> Stream.merge2 s8 
                    |> Stream.merge2 s9 
                    |> Stream.firstArrivals 3 
                    |> c6

        do! Stream.sink n6
                |> Proc.runInStartTime

        // return the simulation results
        return [ResultSource.From ("timer2", timer2, "Timer 2");
                ResultSource.From ("timer3", timer3, "Timer 3"); 
                ResultSource.From ("timer4", timer4, "Timer 4"); 
                ResultSource.From ("timer5", timer5, "Timer 5"); 
                ResultSource.From ("projCompletion", 
                    projCompletionTimer, 
                    "Project Completion Timer")]
                        |> ResultSet.create
    }

    /// the model summary
    let modelSummary =
        model |> Simulation.map ResultSet.summary
