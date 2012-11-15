open Definitions
open Util
open Constants
open Netgraphics

type game = State of (player * player) | Null 
type player = (steammon list * inventory)
type steammon = (steamtypes * (attack * int) list * stats * status)
type steamtypes = Single of steamtype | Dual of steamtype * steamtype
type steamtype = 
	| Normal
	| Fire
	| Water
	| Grass
	| Electric
	| Ice
	| Fighting
	| Poison
	| Ground
	| Flying
	| Psychic
	| Bug
	| Rock
	| Ghost
	| Dragon
	| Dark
	| Steel
type attack = steamtype * int * 
type item =
	| Ether
	| MaxPotion
	| Revive
	| FullHeal
	| XAttack
	| XDefense
	| XSpeed
	| XAccuracy

let game_datafication g =
	failwith "not implemented"
	
let game_from_data game_data = 
	failwith "not implemented"

let handle_step g ra ba =
	failwith "implement me!"

let init_game () =
	failwith "implement me!"