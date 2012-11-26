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

let attack_tbl = Hashtbl.create 300
let steammon_tbl = Hashtbl.create 300
let attack_of_string str = Hashtbl.find attack_tbl str
let steammon_of_string str = Hashtbl.find steammon_tbl str
let attack_lst = Hashtbl.fold (fun k v acc -> v::acc) attack_tbl []

let game_datafication (g:game) : game_status_data =
  match g with {State.game_data = data} -> data  
  
let game_from_data (game_data:game_status_data) : game  = 
  let g = State.create in
  g.State.game_data <- game_data

let handle_step (g:game) (ra:command) (ba:command) : game_output =
  let helper (team: color) (c:command) (s:state) : request =
    match c with
    | Action act -> match act with
      | PickSteammon str -> 
        State.add_steammon s team (Hashtbl.find steammon_tbl str);
        if State.team_full s team then
          PickRequest(team, s.game_data, attack_lst attack_tbl, s.undrafted_steammon)
        else
          PickInventoryRequest(s.game_data)
      | PickInventory inventory ->
        State.set_inventory s team inventory;
        StarterRequest(s.game_data)
      | SelectStarter str ->
        State.switch_steammon s team (Hashtbl.find steammon_tbl str);
        ActionRequest(s.game_data)
      | SwitchSteammon str ->
        State.switch_steammon s team (Hashtbl.find steammon_tbl str);
        ActionRequest(s.game_data)
      | UseItem (item, str) ->
        State.use_item item (Hashtbl.find steammon_tbl str);
        ActionRequest(s.game_data)
      | UseAttack str ->
        State.attack s team (Hashtbl.find attack_tbl);
        if State.active_fainted s team then 
          StarterRequest(s.game_data)
        else
          ActionRequest(s.game_data)
    | _ -> None (* ignores command and returns current state *) in
  let redRequest = helper Red ra g in
  let blueRequest = helper Blue ba g in
  let result = State.game_result g in
  (result, g, redReqest, blueRequest)

let init_game () =
  let alines = read_lines "./game/attack.txt" in
  (* construct an attack list *)
  let attacks = 
    List.fold_left (fun acc line -> 
      match wordify line with
      | [n; e; pp; pow; acc; crit; eff; eff_chance] ->
        let attack =
          {name = n; element = type_of_string e; max_pp = int_of_string pp;
          pp_remaining = int_of_string pp;
          power = int_of_string pow; accuracy = int_of_string acc;
          crit_chance = int_of_string crit;
          effect = (effect_of_num eff) * eff_chance} in
        Hashtbl.add attack_tbl n attack;
        attack :: acc
      | _ -> failwith "incorrect attack input format"
    ) [] alines in
  let slines = read_lines "./game/attack.txt" in
  (* construct a steammon list *)
  let steammon = 
    List.fold_left (fun acc line -> 
      match wordify line with
      | [s; hp; t1; t2; a1; a2; a3; a4; a; sa; d; sd; sp]  ->
        let smon = {species = s; curr_hp = hp; max_hp = hp;
          first_type = match t1 with "Nothing" -> None | _ -> type_of_string t1;
          second_type = match t2 with "Nothing" -> None | _ -> type_of_string t2;
          first_attack = attack_of_string a1; second_attack = attack_of_string a2;
          third_attack = attack_of_string a3; fourth_attack = attack_of_string a4;
          attack = int_of_string a; spl_attack = int_of_string sa;
          defense = int_of_string d; spl_defense = int_of_string sd;
          speed = int_of_string sp;
          status = []; mods = {attack_mod = 0; speed_mod = 0; defense_mod = 0;
          accuracy_mod = 0}} in
        Hashtbl.add steammon_tbl s smon;
        smon :: acc
      | _ -> failwith "incorrect attack input format"
    ) [] slines in
  let first_pick = 
    if (Random.float 1) > 0.5 then Red else Blue in
  let new_game = State.create steammon_tbl in
  (new_game, first_pick, attacks, steammon)
