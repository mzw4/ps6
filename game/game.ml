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

let game = State.create ()

let game_datafication (g:game) : game_status_data =
  match g with {game_data = data} -> data  
  
let game_from_data (game_data:game_status_data) : game  = 
  game.game_data <- game_data

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
  let alines = read_lines "./game/attack.txt" in
  let attacks = 
    List.fold_left (fun acc line -> 
      match Str.split (Str.regexp_string " ") line with
      | [n; e; pp; pow; acc; crit; eff; eff_chance] ->
        {name = name; element = e; max_pp = pp; pp_remaining = pp; power = pow;
        accuracy = acc; crit_chance = crit; effect = eff} :: acc
      | _ -> failwith "incorrect attack input format"
    ) [] alines in
  let slines = read_lines "./game/attack.txt" in
  let steammon = 
    List.fold_left (fun acc line -> 
      match Str.split (Str.regexp_string " ") line with
      | [n; e; pp; pow; acc; crit; eff; eff_chance] ->
        {name = name; element = e; max_pp = pp; pp_remaining = pp; power = pow;
        accuracy = acc; crit_chance = crit; effect = eff} :: acc
      | _ -> failwith "incorrect attack input format"
    ) [] slines in

  
