open Definitions
open Util
open Constants
open Netgraphics
open State

type game = State.state 

let attack_tbl = Hashtbl.create 300
let steammon_tbl = Hashtbl.create 300
let attack_of_string str = Hashtbl.find attack_tbl str
let steammon_of_string str = Hashtbl.find steammon_tbl str
let attack_set = Hashtbl.fold (fun k v acc -> v::acc) attack_tbl []
let steammon_pool = ref []

let print_steammon_pool () = List.fold_left (fun acc x -> print_endline (x.species)) () !steammon_pool

let game_from_data (game_data:game_status_data) : game = 
  let g = State.create () in
  g.game_data <- game_data; g

let game_datafication (g:game) : game_status_data = g.game_data
  
(*add gui updates *)
let handle_step (g:game) (ra:command) (ba:command) : game_output =
  print_endline "HANDLIN";
  let helper (team: color) (c:command) (s:game) : command option =
    match c with
    | Action act -> 
      let request = (match act with
      | PickSteammon str ->
        State.add_steammon s team (Hashtbl.find steammon_tbl str);
        Netgraphics.send_update
          (UpdateSteammon(str, State.get_curr_hp s team str,
          State.get_max_hp s team str, team));
        if State.team_full s team then
          PickInventoryRequest(s.game_data)
        else
          PickRequest(team, s.game_data, attack_set, !steammon_pool)
      | PickInventory inventory ->
        State.set_inventory s team inventory;
        StarterRequest(s.game_data)
      | SelectStarter str ->
        State.switch_steammon s team (Hashtbl.find steammon_tbl str);
        Netgraphics.send_update (SetChosenSteammon(str));
        ActionRequest(s.game_data)
      | SwitchSteammon str ->
        State.switch_steammon s team (Hashtbl.find steammon_tbl str);
        Netgraphics.send_update (SetChosenSteammon(str));
        ActionRequest(s.game_data)
      | UseItem (item, str) -> (* remember status effect gui updates *)
        State.use_item s team item (Hashtbl.find steammon_tbl str);
        ActionRequest(s.game_data)
      | UseAttack str ->
        State.attack s team (Hashtbl.find attack_tbl str);
        if State.active_fainted s team then 
          StarterRequest(s.game_data)
        else
          ActionRequest(s.game_data)) in
      Some(Request(request))
    | _ -> 
      if not (State.team_full s team) then
        Some(Request(PickRequest(team, s.game_data, attack_set, !steammon_pool)))
      else None (* ignores command *) in
  (* determines which teams has action priority *)
  let priority_team = 
    let faster = State.faster_team g in
    match ra, ba with 
    | ((Action act1), (Action act2)) -> begin
      match act1, act2 with 
      | ((SwitchSteammon str), (SwitchSteammon str2)) -> faster
      | ((SwitchSteammon str),_) -> Red
      | (_,(SwitchSteammon str)) -> Blue
      | ((UseItem (item, str)), (UseItem (item2, str2))) -> faster
      | ((UseItem (item, str)), _) -> Red
      | (_, (UseItem (item, str))) -> Blue
      | _ -> faster end
    | _ -> faster in
  let (redRequest, blueRequest) = 
    match priority_team with 
    | Red -> 
      let r1 = helper Red ra g in
      let r2 = helper Blue ba g in (r1, r2)
    | Blue ->
      let r1 = helper Blue ba g in
      let r2 = helper Red ra g in (r2, r1) in
  let result = State.game_result g in

  State.print_steammon g; (* delete *)
  State.print_inventory g;
    (match redRequest with
    | Some(Request(PickRequest (_,_,_,_))) -> print_endline "It's a red pick request!"
    | Some(Request(StarterRequest (_))) -> print_endline "It's a red starter request!"
    | _ -> print_endline "other red request");
    
    (match blueRequest with
    | Some(Request(PickRequest (_,_,_,_))) -> print_endline "It's a blue pick request!"
    | Some(Request(StarterRequest (_))) -> print_endline "It's a blue starter request!"
    | None -> print_endline "blue does nothing!"
    | _ -> print_endline "other blue request");

  (result, g.game_data, redRequest, blueRequest)

let init_game () =
  let alines = read_lines "./attack.txt" in
  (* construct an attack list *)
  let attacks = 
    List.fold_left (fun acc line -> 
      match wordify line with
      | [n; e; pp; pow; accu; crit; eff; eff_chance] ->
        let attack =
          {name = n; element = type_of_string e; max_pp = int_of_string pp;
          pp_remaining = int_of_string pp;
          power = int_of_string pow; accuracy = int_of_string accu;
          crit_chance = int_of_string crit;
          effect =
            ((effect_of_num (int_of_string eff)), int_of_string eff_chance)} in
        Hashtbl.add attack_tbl n attack;
        attack :: acc
      | _ -> failwith "incorrect attack input format"
    ) [] alines in
  let slines = read_lines "./steammon.txt" in
  (* construct a steammon list *)
  let steammon = 
    List.fold_left (fun acc line -> 
      match wordify line with
      | [s; hp; t1; t2; a1; a2; a3; a4; a; sa; d; sd; sp]  ->
        let smon = {
          species = s; curr_hp = int_of_string hp; max_hp = int_of_string hp;
          first_type =
           (match t1 with "Nothing" -> None | _ -> Some(type_of_string t1));
          second_type =
           (match t2 with "Nothing" -> None | _ -> Some(type_of_string t2));
          first_attack = attack_of_string a1;
          second_attack = attack_of_string a2;
          third_attack = attack_of_string a3;
          fourth_attack = attack_of_string a4;
          attack = int_of_string a; spl_attack = int_of_string sa;
          defense = int_of_string d; spl_defense = int_of_string sd;
          speed = int_of_string sp;
          status = []; mods = {attack_mod = 0; speed_mod = 0; defense_mod = 0;
          accuracy_mod = 0}} in
        Hashtbl.add steammon_tbl s smon;
        smon :: acc
      | _ -> failwith "incorrect attack input format"
    ) [] slines in
  steammon_pool := steammon;
  let first_pick = 
    if (Random.float 1.) > 0.5 then Red else Blue in
  let new_game = State.create () in
  Netgraphics.add_update (InitGraphics);
  (new_game, first_pick, attacks, steammon)
