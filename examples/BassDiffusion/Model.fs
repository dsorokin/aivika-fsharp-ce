
(* Example: an agent-based version of the classical Bass Diffusion model. 
   You can find a similar model in the documentation of 
   AnyLogic [http://www.xjtek.com] 

The model describes a product diffusion process. Potential adopters of a product 
are influenced into buying the product by advertising and by word of mouth from 
adopters, those who have already purchased the new product. Adoption of a new 
product driven by word of mouth is likewise an epidemic. Potential adopters come 
into contact with adopters through social interactions. A fraction of these contacts 
results in the purchase of the new product. The advertising causes a constant 
fraction of the potential adopter population to adopt each time period. *)

#nowarn "40"

namespace Simulation.Aivika.Examples

open System
open System.Collections.Generic

open Simulation.Aivika
open Simulation.Aivika.Results

module Model =

    let n = 100     // the number of agents

    let advertisingEffectiveness = 0.011
    let contactRate = 100.0
    let adoptionFraction = 0.015

    let specs =
        { StartTime = 0.0; StopTime = 8.0; DT = 0.1;
          Method = RungeKutta4; 
          GeneratorType = StrongGenerator }

    type PersonContext =
        { PotentialAdopters: int ref;
          Adopters: int ref;
          Persons: List<Person> }

    and Person (ctx: PersonContext, agent: Agent) =
    
        let rec potentialAdopter =
            { new AgentState (agent) with
                member x.Activate () = eventive {
                    incr ctx.PotentialAdopters

                    // create a timeout that will hold while the state is active 

                    let! t = Parameter.randomExponential 
                                (1.0 / advertisingEffectiveness)
                                    |> Parameter.lift

                    do! potentialAdopter
                            |> AgentState.addTimeout t 
                                (AgentState.select adopter)
                }
            
                member x.Deactivate () = eventive {
                    decr ctx.PotentialAdopters
                }
            }
        and adopter =
            { new AgentState (agent) with
                member x.Activate () = eventive {
                    incr ctx.Adopters

                    // create a timer that will hold while the state is active

                    let t = Parameter.randomExponential 
                                (1.0 / contactRate)
                                    |> Parameter.lift

                    let m = 
                        eventive {
                            let! i = Parameter.randomUniformInt 
                                        0 (ctx.Persons.Count - 1)
                                            |> Parameter.lift

                            do! ctx.Persons.[i].Buy ()
                        }

                    do! adopter |> AgentState.addTimer t m
                }
            
                member x.Deactivate () = eventive {
                    decr ctx.Adopters
                }
            }

        member x.Agent = agent
        member x.PotentialAdopter = potentialAdopter
        member x.Adopter = adopter
        
        member private x.Buy () = eventive {

            let! st = Agent.selectedState agent

            if st = Some potentialAdopter then

                let! x = Parameter.randomTrue adoptionFraction
                            |> Parameter.lift

                if x then 
                    do! AgentState.select adopter
        }

        member x.Init () = eventive {
            ctx.Persons.Add (x)
            do! AgentState.select potentialAdopter
        }

    let model: Simulation<ResultSet> = simulation {

        let ctx =
            { PotentialAdopters = ref 0;
              Adopters = ref 0;
              Persons  = List<_> () }

        for i = 1 to n do
            let! agent = Agent.create
            let person = Person (ctx, agent)
            do! person.Init () |> Eventive.runInStartTime

        return 
            [ResultSource.From ("potentialAdopters", 
                ctx.PotentialAdopters, "Potential Adopters");
             ResultSource.From ("adopters", 
                ctx.Adopters, "Adopters")]
                    |> ResultSet.create
    }
