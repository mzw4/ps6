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

type game = phase * State.state

let game_datafication g =
	failwith "not implemented"
	
let game_from_data game_data = 
	failwith "not implemented"

let handle_step (g:game) (ra: command) (ba: command) : game_output =
	(phase, state) = g
	match phase with
	| Init -> begin
		
		end
	| Draft -> begin
		
		end
	| Inventory -> begin
		
		end
	| Battle -> begin
		
		end

let init_game () =
	failwith "implement me!"
