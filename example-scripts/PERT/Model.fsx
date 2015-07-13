
(* Example: Analysis of a PERT-Type Network.

[1] A. Alan B. Pritsker, Simulation with Visual SLAM and AweSim, 2nd ed.
[2] Труб И.И., Объектно-ориентированное моделирование на C++: Учебный курс. - СПб.: Питер, 2006

 *)

#nowarn "40"

#I "../../bin"
#r "../../bin/Simulation.Aivika.dll"
#r "../../bin/Simulation.Aivika.Results.dll"

open Simulation.Aivika
open Simulation.Aivika.Queues
open Simulation.Aivika.Results

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
