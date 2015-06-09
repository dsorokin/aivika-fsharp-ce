
#load "Model.fsx"

open Simulation.Aivika
open Simulation.Aivika.Results

let specs = Model.specs
let model = Model.model Model.defaultParams

ResultSet.printInStopTime specs model
