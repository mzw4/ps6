open Definitions
open Util
open Constants
open Netgraphics

type phase =
  | Init
  | Draft
  | Inventory
  | Battle

type game = phase * State.state


let game_datafication (g:game) : game_status_data =
	(phase, state) = g in
	
	
	
let game_from_data (game_data:game_status_data) : game  = 
	failwith "not implemented"

let handle_step (g:game) (ra:command) (ba:command) : game_output =
	let (phase, state) = g in
	let helper (c:command) (s:state) : state =
		match c with
		| Action act -> 
			match phase with
			| Draft ->
				match act with
				| 
		| _ -> failwith ""
	in	 
	failwith ""

let init_game () =
	failwith "implement me!"
