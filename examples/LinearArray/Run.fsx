
#load "Model.fsx"

open Simulation.Aivika
open Simulation.Aivika.Results

ResultSet.printInStopTime Model.specs (Model.model 51)
