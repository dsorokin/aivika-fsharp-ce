
(* Example: Port Operations

It is described in different sources [1, 2]. So, this is chapter 12 of [2] and section 6.13 of [1].

A port in Africa is used to load tankers with crude oil for overwater shipment.
The port has facilities for loading as many as three tankers simultaneously.
The  tankers, which arrive at the port every 11 +/- 7 hours, are of three different
types. The relative frequency of the various types, and their loading time
requirements, are as follows:

Type      Relative Frequency      Loading Time, Hours
  1              0.25                   18 +/- 2
  2              0.55                   24 +/- 3
  3              0.20                   36 +/- 4

There is one tug at the port. Tankers of all types require the services of this tug
to move into a berth, and later to move out of a berth. When the tug is available,
any berthing or de-berthing activity takes about one hour. Top priority is given to
the berthing activity.

A shipper is considering bidding on a contract to transport oil from the port to
the United Kingdom. He has determined that 5 tankers of a particular type would
have to be committed to this task to meet contract specifications. These tankers
would require 21 +/- 3 hours to load oil at the port. After loading and de-berthing,
they would travel to the United Kingdom, offload the oil, and return to the port for
reloading. Their round-trip travel time, including offloading, is estimated to be
240 +/- hours.

A complicated factor is that the port experiences storms. The time between
the onset of storms is exponentially distributed with a mean of 48 hours and a.
storm lasts 4 +/- 2 hours. No tug can start an operation until a storm is over.

Before the port authorities can commit themselves to accommodating the
proposed 5 tankers, the effect of the additional port traffic on the in-port residence
time of the current port users must be determined. It is desired to simulate the
operation of the port for a one-year period (= 8640 hours) under the proposed new
commitment to measure in-port residence time of the proposed additional tankers,
as well as the three types of tankers which already use the port. All durations
given as ranges are uniformly distributed.

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

    StartTime=0.0; StopTime=8760.0; DT=0.1; 
    Method=RungeKutta4; GeneratorType=StrongGenerator
}

type Tunker = { LoadingTime: float; Type: int }

/// the simulation model
let model = simulation {

    let portTime = [| for i = 1 to 4 do yield ref SamplingStats.emptyFloats |]

    let! berth =
        Resource.createUsingFCFS 3
            |> Eventive.runInStartTime

    let! tug =
        Resource.createUsingFCFS 1
            |> Eventive.runInStartTime

    let tunkers13 = Stream.randomUniform 4.0 18.0
    let tunkers4  = Stream.randomUniform 48.0 48.0
                        |> Stream.take 5

    let rec arv1 = 
        eventive {
            let! loadingTime = 
                Parameter.randomUniform 16.0 20.0 
                    |> Parameter.lift
            let t = { LoadingTime = loadingTime; Type = 1 }
            do! Proc.run (port t)
        }
    and arv2 =
        eventive {
            let! loadingTime = 
                Parameter.randomUniform 21.0 27.0 
                    |> Parameter.lift
            let t = { LoadingTime = loadingTime; Type = 2 }
            do! Proc.run (port t)
        }
    and arv3 =
        eventive {
            let! loadingTime = 
                Parameter.randomUniform 32.0 40.0 
                    |> Parameter.lift
            let t = { LoadingTime = loadingTime; Type = 3 }
            do! Proc.run (port t)
        }
    and arv4 =
        eventive {
            let! loadingTime = 
                Parameter.randomUniform 18.0 24.0 
                    |> Parameter.lift
            let t = { LoadingTime = loadingTime; Type = 4 }
            do! Proc.run (port t)
        }
    and port (t: Tunker) =
        proc {
            let! t0 = Dynamics.time |> Dynamics.lift
            do! Resource.request berth
            do! Resource.request tug
            do! Proc.hold 1.0
            do! Resource.release tug
            do! Proc.hold t.LoadingTime
            do! Resource.request tug
            do! Proc.hold 1.0
            do! Resource.release tug
            do! Resource.release berth
            let! t1 = Dynamics.time |> Dynamics.lift
            portTime.[t.Type - 1] :=
                !(portTime.[t.Type - 1]) |> SamplingStats.add (t1 - t0) 
            if t.Type = 4 then
                do! proc {
                        do! Proc.randomUniform_ 216.0 264.0
                        do! Eventive.lift arv4
                    } |> Proc.run
                      |> Eventive.lift
        }
    and storm =
        proc {
            while true do
                do! Proc.randomExponential_ 48.0
                do! Resource.decCount 1 tug
                do! Proc.randomUniform_ 2.0 6.0
                do! Resource.incCount 1 tug
                        |> Eventive.lift
        }
    do! proc {
            for x in tunkers13 do
                let! p = 
                    Parameter.randomUniform 0.0 1.0
                        |> Parameter.lift        
                if p <= 0.25 then
                    do! Eventive.lift arv1 
                elif p <= 0.25 + 0.55 then
                    do! Eventive.lift arv2
                else
                    do! Eventive.lift arv3
                 
        } |> Proc.runInStartTime
    do! proc {
            for x in tunkers4 do
                do! Eventive.lift arv4
        } |> Proc.runInStartTime
    do! storm |> Proc.runInStartTime

    // return the simulation results
    return [ResultSource.From ("portTime", portTime, 
                "Port Time");
            ResultSource.From ("berth", berth, 
                "Berth");
            ResultSource.From ("tug", tug, 
                "Tug")]
                    |> ResultSet.create
}

/// the model summary
let modelSummary =
    model |> Simulation.map ResultSet.summary
