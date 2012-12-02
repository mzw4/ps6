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
let first_picker = ref Red

let print_steammon_pool () = List.fold_left (fun acc x -> print_endline (x.species)) () !steammon_pool

let game_from_data (game_data:game_status_data) : game = 
  let g = State.create () in
  g.game_data <- game_data; g

let game_datafication (g:game) : game_status_data = g.game_data
  
(*add gui updates *)
let handle_step (g:game) (ra:command) (ba:command) : game_output =
  print_endline "HANDLIN";
  let performStep (team: color) (c: command) (s:game) : unit =
    print_endline( "performing step for " ^ (color_to_string team));
      if team = Red then (match c with
      | Action(PickSteammon str) -> print_endline ("it was pick for Red")
      | _ -> print_endline ("not a pick for red"));
    match c with
    | Action act ->
      print_endline "Matching action";
      (match act with
      | PickSteammon str ->
        print_endline ("adding " ^ str ^ " for " ^ (color_to_string team));
        State.add_steammon s team (Hashtbl.find steammon_tbl str);
        Netgraphics.send_update
          (UpdateSteammon(str, State.get_curr_hp s team str,
          State.get_max_hp s team str, team));
      | PickInventory inventory ->
        State.set_inventory s team inventory;
      | SelectStarter str ->
        State.switch_steammon s team (Hashtbl.find steammon_tbl str);
        Netgraphics.send_update (SetChosenSteammon(str));
      | SwitchSteammon str ->
        State.switch_steammon s team (Hashtbl.find steammon_tbl str);
        Netgraphics.send_update (SetChosenSteammon(str));
      | UseItem (item, str) ->
        State.use_item s team item (Hashtbl.find steammon_tbl str);
      | UseAttack str ->
        State.attack s team (Hashtbl.find attack_tbl str);)
    | _ -> print_endline "OMG"; () in
  let nextRequest (team: color) (c:command) (s:game) : command option =
    match c with
    | Action act ->
      (match act with
      | PickSteammon str ->
        if State.num_in_party s team = 1 && (* identifies first picker *)
           State.num_in_party s (opposite_color team) = 0 then
          first_picker := team;
        if (State.num_in_party s team = cNUM_PICKS) then
          Some(Request(PickInventoryRequest(s.game_data)))
        else if (team = !first_picker && ((State.num_in_party s team) mod 3 = 0 ||
                (State.num_in_party s team = 1))) ||
                (team <> !first_picker && (State.num_in_party s team) mod 2 = 0) then
          None
        else
          Some(Request(PickRequest(team, s.game_data, attack_set, !steammon_pool)))
      | PickInventory inventory ->
        Some(Request(StarterRequest(s.game_data)))
      | SelectStarter str ->
        Some(Request(ActionRequest(s.game_data)))
      | SwitchSteammon str ->
        Some(Request(ActionRequest(s.game_data)))
      | UseItem (item, str) ->
        Some(Request(ActionRequest(s.game_data)))
      | UseAttack str ->
        if State.active_fainted s team then
          Some(Request(StarterRequest(s.game_data)))
        else
          Some(Request(ActionRequest(s.game_data))))
    | DoNothing ->
      if (State.num_in_party s team = cNUM_PICKS) then
        Some(Request(ActionRequest(s.game_data)))
      else if (State.num_in_party s team) = 0 ||
         (team = !first_picker && ((State.num_in_party s team) = 1 ||
         (State.num_in_party s team) mod 3 = 0) &&
         (State.num_in_party s (opposite_color team)) mod 2 = 0) ||
         (team <> !first_picker && (State.num_in_party s team) mod 2 = 0 &&
         ((State.num_in_party s (opposite_color team)) mod 3 = 0 ||
         (State.num_in_party s (opposite_color team)) = 1 ||
         (State.num_in_party s (opposite_color team)) = cNUM_PICKS)) then
        Some(Request(PickRequest(team, s.game_data, attack_set, !steammon_pool)))
      else
        None
    | _ -> None (* ignores command *) in
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
  (match priority_team with
  | Red ->
    performStep Red ra g;
    performStep Blue ba g
  | Blue ->
    performStep Blue ba g;
    performStep Red ra g);
  let redRequest = nextRequest Red ra g in
  let blueRequest =  nextRequest Blue ba g in
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
    if Random.bool () = true then Red else Blue in
  print_endline ((color_to_string first_pick) ^ "gets first pick!");
  let new_game = State.create () in
  Netgraphics.add_update (InitGraphics);
  (new_game, first_pick, attacks, steammon)
