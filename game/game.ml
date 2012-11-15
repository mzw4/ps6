open Definitions
open Util
open Constants
open Netgraphics
open State

type phase =
  | Init
  | Draft
  | Inventory
  | Battle

type game = phase * state | Null 

let game_datafication g =
	failwith "not implemented"
	
let game_from_data game_data = 
	failwith "not implemented"

let handle_step g ra ba =
	failwith "implement me!"

let init_game () =
	failwith "implement me!"
