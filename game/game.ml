open Definitions
open Util
open Constants
open Netgraphics

type phase =
  | Init
  | Draft
  | Inventory
  | Battle

type game = State.state


let game_datafication (g:game) : game_status_data =
	failwith "not implemented"
	
	
let game_from_data (game_data:game_status_data) : game  = 
	failwith "not implemented"

let handle_step (g:game) (ra:command) (ba:command) : game_output =
	let (phase, state) = g in
	let helper (c:command) (s:state) : state =
		match c with
		| Action act -> match act with
			| PickSteammon str -> 
				(* implement this *)
			| PickInventory inventory ->
				(* implement this *)
			| SelectStarter str ->
				(* implement this *)
			| SwitchSteammon str ->
				(* implement this *)
			| UseItem (item, str) ->
				(* implement this *)	
			| UseAttack str ->
		| _ -> s (* ignores command and returns current state *)
	in	 
	failwith ""

let init_game () =
	failwith "implement me!"
