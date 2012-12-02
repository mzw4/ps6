open Definitions
open Util
open Constants
open Netgraphics

type state = {mutable game_data: game_status_data}

let create () : state = 
  {game_data = (([], [0;0;0;0;0;0;0;0]), ([], [0;0;0;0;0;0;0;0]))}

let set_game_data (st: state) (game_data: game_status_data) : unit =
  st.game_data <- game_data
  

let print_steammon st =
  let ((r_steammon, r_inventory), (b_steammon, b_inventory)) = st.game_data in
  let rmons = List.fold_left (fun acc x ->
    acc ^ " " ^(x.species) ^ " " ^ (string_of_int x.curr_hp) ^ " ") "" r_steammon in
  let bmons = List.fold_left (fun acc x ->
    acc ^" "^ (x.species) ^ " " ^ (string_of_int x.curr_hp) ^ " ") "" b_steammon in
  print_endline ("Red: " ^ rmons);
  print_endline ("Blue: " ^ bmons)

let print_inventory st = 
  let ((r_steammon, r_inventory), (b_steammon, b_inventory)) = st.game_data in
  let print inventory =
    List.fold_left (fun acc x -> acc ^ " " ^ (string_of_int x)) "" inventory in
  print_endline ("Red inventory:" ^ (print r_inventory));
  print_endline ("Blue inventory:" ^ (print b_inventory))


(* Indicates if the steammon has already been drafted *)
let already_selected (st: state) (steammon: steammon) : bool = 
  let ((r_steammon, r_inventory), (b_steammon, b_inventory)) = st.game_data in
  let selected lst = 
    List.fold_left (fun acc s ->
    if s.species = steammon.species then true
    else acc) false lst in
  selected r_steammon ||  selected b_steammon

(* Indicates the number of steammon currently drafted into a team *)
let num_in_party (st: state) (team: color) : int =
  let ((r_steammon, r_inventory), (b_steammon, b_inventory)) = st.game_data in
  match team with 
  | Red -> List.length r_steammon
  | Blue -> List.length b_steammon

(* Indicates if a team is full and finished with the draft step *)
let team_full (st: state) (team: color) : bool  = 
  let ((r_steammon, r_inventory), (b_steammon, b_inventory)) = st.game_data in
  let r_num = List.length r_steammon in
  let b_num = List.length b_steammon in
  if r_num > cNUM_PICKS || b_num > cNUM_PICKS then failwith "drafted too many pkmns wtf"
  else (team = Red && r_num = cNUM_PICKS) || (team = Blue && b_num = cNUM_PICKS)

(* Indicates if the specified steammon is currently active *)
let is_active (st: state) (team: color) (steammon: steammon) : bool =
  let ((r_steammon, r_inventory), (b_steammon, b_inventory)) = st.game_data in
  match team with 
  | Red -> (List.hd r_steammon).species = steammon.species
  | Blue -> (List.hd b_steammon).species = steammon.species

(* Indicates which team's active steammon is faster *)
let faster_team (st: state) : color = 
  let ((r_steammon, r_inventory), (b_steammon, b_inventory)) = st.game_data in
  if List.length r_steammon = 0 || List.length b_steammon = 0 then begin
    if Random.float 1. > 0.5 then Red
    else Blue end
  else begin
    let r_speed = (List.hd r_steammon).speed in
    let b_speed = (List.hd b_steammon).speed in
    if r_speed > b_speed then Red
    else if b_speed > r_speed then Blue
    else if Random.float 1. > 0.5 then Red
    else Blue end

(* Indicates if the active steammon of a team has fainted *)
let active_fainted (st: state) (team: color) : bool = 
  let ((r_steammon, r_inventory), (b_steammon, b_inventory)) = st.game_data in    
  match team with
  | Red -> (List.hd r_steammon).curr_hp <= 0
  | Blue -> (List.hd b_steammon).curr_hp <= 0
  
(* Indicates if the inventory contains the specified item *)
let inventory_contains (st: state) (team: color) (item: item) : bool = 
  let ((r_steammon, r_inventory), (b_steammon, b_inventory)) = st.game_data in
  let contains (inventory: int list) (i: item) = 
    match inventory with 
    | [ethers; maxpots; revs; fullhs; xatt; xdef; xspd; xacc] -> 
      (match i with 
      | Ether -> ethers > 0
      | MaxPotion -> maxpots > 0
      | Revive -> revs > 0
      | FullHeal -> fullhs > 0
      | XAttack -> xatt > 0
      | XDefense -> xdef > 0
      | XSpeed -> xspd > 0
      | XAccuracy -> xacc > 0) 
    | _ -> failwith "invalid inventory" in
  match team with 
  | Red -> contains r_inventory item
  | Blue -> contains b_inventory item

(* Indicates if there is a winner and the game is over, otherwise returns None *)
let game_result (st: state) : game_result option = 
  let ((r_steammon, r_inventory), (b_steammon, b_inventory)) = st.game_data in
  let all_fainted (steammon: steammon list) =
    if List.length steammon > 0 then
      List.fold_left (fun acc s ->
        if s.curr_hp <> 0 then false
        else acc) true steammon
    else false in
  if all_fainted r_steammon then Some(Winner(Blue))
  else if all_fainted b_steammon then Some(Winner(Red))
  else None

(* Adds a steammon to a team's steammon list. *)
(* Throws an exception if that steammon is not in the draftable list*)
let add_steammon (st: state) (team: color) (steammon: steammon) : unit =
  let ((r_steammon, r_inventory), (b_steammon, b_inventory)) = st.game_data in
  let helper (steammon_lst: steammon list) : steammon list =
    if not (already_selected st steammon) then steammon::steammon_lst
    else failwith "steammon is no longer available" in
  match team with 
  | Red -> set_game_data st
    ((helper r_steammon, r_inventory), (b_steammon, b_inventory))
  | Blue -> set_game_data st
    ((r_steammon, r_inventory), (helper b_steammon, b_inventory))

(* Switches a steammon so it appears at the head of a the steammon list. *)
(* Throws an exception if that steammon is not in the list. *)
let switch_steammon (st: state) (team: color) (s: steammon) : unit =
print_endline ((color_to_string team) ^ " switching to " ^ s.species);
  let (red_data, blue_data) = st.game_data in
  let helper (t_data: team_data) : team_data =
    let (lst, inventory) = t_data in
    if (List.exists (fun x -> x.species = s.species && x.curr_hp > 0) lst) then
      let tl = List.filter (fun x -> x.species <> s.species) lst in 
      let starter = 
  {species = s.species; 
        curr_hp = s.curr_hp; 
        max_hp = s.max_hp; 
        first_type = s.first_type; 
        second_type = s.second_type; 
        first_attack = s.first_attack; 
        second_attack = s.second_attack; 
        third_attack = s.third_attack; 
        fourth_attack = s.fourth_attack; 
        attack = s.attack; 
        spl_attack = s.spl_attack; 
        defense = s.defense; 
        spl_defense = s.spl_defense; 
        speed = s.speed; 
        status = (List.filter (fun x -> x <> Confused) s.status); 
        mods = {
  attack_mod = 0;
  speed_mod = 0;
  defense_mod = 0;
  accuracy_mod = 0;}} in
      Netgraphics.add_update (SetStatusEffects(starter.species, starter.status));
      Netgraphics.add_update (SetChosenSteammon(starter.species));
      Netgraphics.add_update (Message((color_to_string team) ^
        " switched in " ^ starter.species));
      (starter::tl, inventory)
    else
      (Netgraphics.add_update (Message("Cannot switch in that steammon!"));
      (lst, inventory)) in  
  match team with 
  | Red -> set_game_data st ((helper red_data), blue_data)
  | Blue -> set_game_data st (red_data, (helper blue_data))

(* Gets the max hp of a steammon *)
let get_max_hp (st: state) (team: color) (species: string) : int =
  let ((r_steammon, r_inventory), (b_steammon, b_inventory)) = st.game_data in
  let hp mons = List.fold_left (fun acc x -> 
      if x.species = species then x.max_hp
      else acc) 0 mons in
  match team with 
  | Red -> hp r_steammon
  | Blue -> hp b_steammon

(* Gets the current hp of a steammon *)
let get_curr_hp (st: state) (team: color) (species: string) : int =
  let ((r_steammon, r_inventory), (b_steammon, b_inventory)) = st.game_data in
  let chp mons = List.fold_left (fun acc x -> 
     if x.species = species then x.curr_hp
     else acc) 0 mons in
  match team with 
  | Red -> chp r_steammon
  | Blue -> chp b_steammon

(* Sets a player's inventory to the specified one *)
let set_inventory (st: state) (team: color) (inventory: int list) : unit = 
  let ((r_steammon, r_inventory), (b_steammon, b_inventory)) = st.game_data in
  match team with 
  | Red -> set_game_data st ((r_steammon, inventory), (b_steammon, b_inventory))
  | Blue -> set_game_data st ((r_steammon, r_inventory), (b_steammon, inventory))

(* Adds an item to a team's inventory *)  
let add_item (st: state) (team: color) (i: item) : unit =
  let (red_data, blue_data) = st.game_data in
  let helper (t_data: team_data) : team_data =
    let (lst, inventory) = t_data in
    let newInventory = 
    match inventory with
    | [ethers; maxPotions; revives; fullHeals; xAttacks; xDefenses;
      xSpeeds; xAccuracies] ->
      (match i with
        | Ether -> [(ethers + 1) ; maxPotions; revives; fullHeals; xAttacks;
          xDefenses; xSpeeds; xAccuracies]
        | MaxPotion -> [ethers; (maxPotions + 1); revives; fullHeals; xAttacks;
          xDefenses; xSpeeds; xAccuracies]
        | Revive -> [ethers; maxPotions; (revives + 1); fullHeals; xAttacks;
          xDefenses; xSpeeds; xAccuracies]
        | FullHeal -> [ethers; maxPotions; revives; (fullHeals + 1); xAttacks;
          xDefenses; xSpeeds; xAccuracies]
        | XAttack -> [ethers; maxPotions; revives; fullHeals; (xAttacks + 1);
          xDefenses; xSpeeds; xAccuracies]
        | XDefense -> [ethers; maxPotions; revives; fullHeals; xAttacks;
          (xDefenses + 1); xSpeeds; xAccuracies]
        | XSpeed -> [ethers; maxPotions; revives; fullHeals; xAttacks;
          xDefenses; (xSpeeds + 1); xAccuracies]
        | XAccuracy -> [ethers; maxPotions; revives; fullHeals; xAttacks;
          xDefenses; xSpeeds; (xAccuracies + 1)])
    | _ -> failwith "invalid inventory"  in
    (lst, newInventory) in
  match team with
  | Red -> set_game_data st ((helper red_data), blue_data)
  | Blue -> set_game_data st (red_data, (helper blue_data))

    
(* Removes an item to a team's inventory. *)
let remove_item (inventory: int list) (i: item) : int list =
    let newInventory = 
      match inventory with 
      | [ethers; maxPotions; revives; fullHeals; xAttacks; xDefenses;
        xSpeeds; xAccuracies] ->
        let f (i: int) : int =
          if (i > 0) then i - 1
          else 0 in
        (match i with
        | Ether -> [(f ethers) ; maxPotions; revives; fullHeals; xAttacks; xDefenses;
          xSpeeds; xAccuracies]
        | MaxPotion -> [ethers; (f maxPotions); revives; fullHeals; xAttacks; xDefenses;
          xSpeeds; xAccuracies]
        | Revive -> [ethers; maxPotions; (f revives); fullHeals; xAttacks; xDefenses;
          xSpeeds; xAccuracies]
        | FullHeal -> [ethers; maxPotions; revives; (f fullHeals); xAttacks; xDefenses;
          xSpeeds; xAccuracies]
        | XAttack -> [ethers; maxPotions; revives; fullHeals; (f xAttacks); xDefenses;
          xSpeeds; xAccuracies]
        | XDefense -> [ethers; maxPotions; revives; fullHeals; xAttacks; (f xDefenses);
          xSpeeds; xAccuracies]
        | XSpeed -> [ethers; maxPotions; revives; fullHeals; xAttacks; xDefenses;
          (f xSpeeds); xAccuracies]
        | XAccuracy -> [ethers; maxPotions; revives; fullHeals; xAttacks; xDefenses;
          xSpeeds; (f xAccuracies)])
      | _ -> failwith "invalid inventory" in newInventory

(* Processes an attack and calculates the changes in states *)
let attack (st: state) (team: color) (a: attack) : unit = 
  print_endline ((color_to_string team) ^ " attacking with " ^ a.name);
  let (red_data, blue_data) = st.game_data in
  (* booleans for handling statuses *)
  let stuck_if_paralyzed = (Random.int 99) < cPARALYSIS_CHANCE in
  let defrost_if_frozen = (Random.int 99) < cDEFROST_CHANCE in
  let wake_up_if_asleep = (Random.int 99) < cWAKE_UP_CHANCE in
  let attack_self_if_confused = (Random.int 99) < cSELF_ATTACK_CHANCE in
  let snap_out_of_confused = (Random.int 99) < cSNAP_OUT_OF_CONFUSION in
  (* boolean for if an attack is a regular attack or special attack *)
  let is_special = 
    match a.element with
    | Electric
    | Fire 
    | Water
    | Psychic
    | Ghost -> true
    | _ -> false in
  (* sets status *)
  let set_status (s: steammon) (status: status list) : steammon = 
    {species = s.species; 
    curr_hp = s.curr_hp; 
    max_hp = s.max_hp; 
    first_type = s.first_type; 
    second_type = s.second_type; 
    first_attack = s.first_attack; 
    second_attack = s.second_attack;
    third_attack = s.third_attack; 
    fourth_attack = s.fourth_attack; 
    attack = s.attack; 
    spl_attack = s.spl_attack; 
    defense = s.defense; 
    spl_defense = s.spl_defense; 
    speed = s.speed; 
    status = status; 
    mods = s.mods} in
  let add_status (s: steammon) (stat: status) : steammon = 
    let string_of_stat stat =
      match stat with 
      | Confused -> "confused"
      | Poisoned -> "poisoned"
      | Paralyzed -> "paralyzed"
      | Asleep -> "asleep"
      | Frozen -> "frozen" in
    let new_status = 
      if (stat = Confused && (List.mem Confused s.status)) ||
         (stat <> Confused && (
         (List.mem Poisoned s.status) || (List.mem Asleep s.status) || 
         (List.mem Paralyzed s.status) || (List.mem Frozen s.status))) then s.status
      else 
        (Netgraphics.add_update
          (NegativeEffect(s.species ^ " is " ^ (string_of_stat stat) ^ "!",
          opposite_color team, 0));
        stat::s.status) in
     set_status s new_status in
  (* updates mods *)  
  let change_mod (s: steammon) (e:effects) : steammon = 
    let f_up (i: int) : int = 
      if (i = 3) then 3
        else i + 1 in
    let f_down (i: int) : int = 
      if (i = -3) then -3
        else i - 1 in
    let new_mods = 
      match e with
      | SelfAttackUp1 -> 
  {attack_mod = (f_up s.mods.attack_mod); speed_mod = s.mods.speed_mod; 
  defense_mod = s.mods.defense_mod; accuracy_mod = s.mods.accuracy_mod}
      | SelfDefenseUp1 ->         
  {attack_mod = s.mods.attack_mod; speed_mod = s.mods.speed_mod; 
  defense_mod = (f_up s.mods.defense_mod); accuracy_mod = s.mods.accuracy_mod}
      | SelfSpeedUp1 ->
  {attack_mod = s.mods.attack_mod; speed_mod = (f_up s.mods.speed_mod); 
  defense_mod = s.mods.defense_mod; accuracy_mod = s.mods.accuracy_mod}
      | SelfAccuracyUp1 ->
  {attack_mod = s.mods.attack_mod; speed_mod = s.mods.speed_mod; 
  defense_mod = s.mods.defense_mod; accuracy_mod = (f_up s.mods.accuracy_mod)} 
      | OpponentAttackDown1 ->
  {attack_mod = (f_down s.mods.attack_mod); speed_mod = s.mods.speed_mod; 
  defense_mod = s.mods.defense_mod; accuracy_mod = s.mods.accuracy_mod}
      | OpponentDefenseDown1 ->
  {attack_mod = s.mods.attack_mod; speed_mod = s.mods.speed_mod; 
  defense_mod = (f_down s.mods.defense_mod); accuracy_mod = s.mods.accuracy_mod}
      | OpponentSpeedDown1 ->
  {attack_mod = s.mods.attack_mod; speed_mod = (f_down s.mods.speed_mod); 
  defense_mod = s.mods.defense_mod; accuracy_mod = s.mods.accuracy_mod}     
      | OpponentAccuracyDown1  ->
  {attack_mod = s.mods.attack_mod; speed_mod = s.mods.speed_mod; 
  defense_mod = s.mods.defense_mod; accuracy_mod = (f_down s.mods.accuracy_mod)}
      | _ ->
  {attack_mod = s.mods.attack_mod; speed_mod = s.mods.speed_mod; 
  defense_mod = s.mods.defense_mod; accuracy_mod = s.mods.accuracy_mod} in
    {species = s.species; 
    curr_hp = s.curr_hp; 
    max_hp = s.max_hp; 
    first_type = s.first_type; 
    second_type = s.second_type; 
    first_attack = s.first_attack; 
    second_attack = s.second_attack;
    third_attack = s.third_attack; 
    fourth_attack = s.fourth_attack; 
    attack = s.attack; 
    spl_attack = s.spl_attack; 
    defense = s.defense; 
    spl_defense = s.spl_defense; 
    speed = s.speed; 
    status = s.status; 
    mods = new_mods} in
  let mod_attack (s:steammon) : float =
    match s.mods.attack_mod with
    | -3 -> cATTACK_DOWN3
    | -2 -> cATTACK_DOWN2
    | -1 -> cATTACK_DOWN1
    | 1 -> cATTACK_UP1
    | 2 -> cATTACK_UP2
    | 3 -> cATTACK_UP3
    | _ -> 1. in
  let mod_defense (s:steammon) : float = 
    match s.mods.defense_mod with
    | -3 -> cDEFENSE_DOWN3
    | -2 -> cDEFENSE_DOWN2
    | -1 -> cDEFENSE_DOWN1
    | 1 -> cDEFENSE_UP1
    | 2 -> cDEFENSE_UP2
    | 3 -> cDEFENSE_UP3
    | _ -> 1. in      
  (* processes hp changes *)  
  let update_hp (s: steammon) (f : float) : steammon =
    let new_hp = if (s.curr_hp < (int_of_float f)) then 0 
      else (s.curr_hp - (int_of_float f)) in
print_endline ("damange done: " ^ (string_of_float f));
print_endline (s.species ^ " has " ^ (string_of_int new_hp) ^ " hp left.");
    {species = s.species; 
    curr_hp = new_hp; 
    max_hp = s.max_hp; 
    first_type = s.first_type; 
    second_type = s.second_type; 
    first_attack = s.first_attack; 
    second_attack = s.second_attack;
    third_attack = s.third_attack; 
    fourth_attack = s.fourth_attack; 
    attack = s.attack; 
    spl_attack = s.spl_attack; 
    defense = s.defense; 
    spl_defense = s.spl_defense; 
    speed = s.speed; 
    status = s.status; 
    mods = s.mods} in
  (* processes changes in attacker's state *)  
  let attacker_helper (t_data: team_data) : team_data = 
    let (steammon_lst, inventory) = t_data in
    let starter = List.hd steammon_lst in
    let use_pp (s: steammon) : steammon = 
      let f (a: attack) : attack = 
        if (a.pp_remaining <= 0) then a (* don't do anything if pp = 0 *)
        else {name = a.name ; element = a.element ; max_pp = a.max_pp; 
          pp_remaining = (a.pp_remaining - 1); power = a.power; accuracy = 
          a.accuracy; crit_chance = a.crit_chance; effect = a.effect} in
      if a.name = s.first_attack.name then 
        {species = s.species; 
        curr_hp = s.curr_hp; 
        max_hp = s.max_hp; 
        first_type = s.first_type; 
        second_type = s.second_type; 
        first_attack = f (s.first_attack); 
        second_attack = s.second_attack; 
        third_attack = s.third_attack; 
        fourth_attack = s.fourth_attack; 
        attack = s.attack; 
        spl_attack = s.spl_attack; 
        defense = s.defense; 
        spl_defense = s.spl_defense; 
        speed = s.speed; 
        status = s.status; 
        mods = s.mods}
      else if a.name = s.second_attack.name then
        {species = s.species; 
        curr_hp = s.curr_hp; 
        max_hp = s.max_hp; 
        first_type = s.first_type; 
        second_type = s.second_type; 
        first_attack = s.first_attack; 
        second_attack = f (s.second_attack);
        third_attack = s.third_attack; 
        fourth_attack = s.fourth_attack; 
        attack = s.attack; 
        spl_attack = s.spl_attack; 
        defense = s.defense; 
        spl_defense = s.spl_defense; 
        speed = s.speed; 
        status = s.status; 
        mods = s.mods}
      else if a.name = s.third_attack.name then
        {species = s.species; 
        curr_hp = s.curr_hp; 
        max_hp = s.max_hp; 
        first_type = s.first_type; 
        second_type = s.second_type; 
        first_attack = s.first_attack; 
        second_attack = s.second_attack;
        third_attack = f(s.third_attack); 
        fourth_attack = s.fourth_attack; 
        attack = s.attack; 
        spl_attack = s.spl_attack; 
        defense = s.defense; 
        spl_defense = s.spl_defense; 
        speed = s.speed; 
        status = s.status; 
        mods = s.mods}
      else if a.name = s.fourth_attack.name then
        {species = s.species; 
        curr_hp = s.curr_hp; 
        max_hp = s.max_hp; 
        first_type = s.first_type; 
        second_type = s.second_type; 
        first_attack = s.first_attack; 
        second_attack = s.second_attack;
        third_attack = s.third_attack; 
        fourth_attack = f(s.fourth_attack); 
        attack = s.attack; 
        spl_attack = s.spl_attack; 
        defense = s.defense; 
        spl_defense = s.spl_defense; 
        speed = s.speed; 
        status = s.status; 
        mods = s.mods}
      else failwith "Steammon does not have that attack" in
    let process_mods (s: steammon) : steammon = 
      let (status, prob) = a.effect in
        if (prob > Random.int 99) then
          match status with
          | SelfAttackUp1 
          | SelfDefenseUp1 
          | SelfSpeedUp1
          | SelfAccuracyUp1  -> change_mod s status
          | _ -> s
  else s in
    let process_confused (s: steammon) : steammon = 
      Netgraphics.add_update
        (Message(s.species ^ " is confused..."));
      if snap_out_of_confused then begin
        Netgraphics.add_update
          (Message(s.species ^ " snapped out of confusion!"));
        use_pp (set_status s (List.filter (fun x -> x <> Confused) s.status))
        end        
      else if attack_self_if_confused then
        let self_dmg = float_of_int (cSELF_ATTACK_POWER * s.attack / s.defense) in
        let hp_diff =
          if (int_of_float self_dmg) > s.curr_hp then s.curr_hp
          else (int_of_float self_dmg) in
        Netgraphics.add_update (NegativeEffect(
          s.species ^ " hurt itself in confusion!", team, hp_diff));
        Netgraphics.add_update
          (UpdateSteammon(starter.species, starter.curr_hp - hp_diff,
          starter.max_hp, team));
        update_hp s self_dmg
      else use_pp s in  
    let updated_starter =
      print_endline "updating starter";
      let other_status = List.filter (fun x -> x <> Confused) starter.status in 
      if (List.mem Confused starter.status) then
        match other_status with
  | [Frozen] ->
          if (defrost_if_frozen) then
            process_confused (set_status starter [Confused])
          else 
            starter
  | [Paralyzed] ->
          if (stuck_if_paralyzed) then
            starter
          else 
            process_confused (set_status starter [Confused])
  | [Asleep] -> 
          if (wake_up_if_asleep) then
            process_confused (set_status starter [Confused])
          else
            starter
  | [Poisoned] ->
          let pdmg = cPOISON_DAMAGE *. 
            (float_of_int starter.attack) /. (float_of_int starter.defense) in
          let hp_diff = 
            if (int_of_float pdmg) > starter.curr_hp then starter.curr_hp
            else (int_of_float pdmg) in
          Netgraphics.add_update (NegativeEffect(
            starter.species ^ "was hurt by poison!", team, hp_diff));
          Netgraphics.add_update
            (UpdateSteammon(starter.species, starter.curr_hp - hp_diff,
            starter.max_hp, team));
          process_confused (update_hp starter pdmg)
  | _ -> process_confused starter
      else 
        match other_status with
  | [Frozen] ->
          if (defrost_if_frozen) then
            use_pp starter
          else 
            starter
  | [Paralyzed] ->
          if (stuck_if_paralyzed) then
            starter
          else 
            use_pp starter
  | [Asleep] ->
          if (wake_up_if_asleep) then
            use_pp starter
          else
            starter
  | [Poisoned] ->
          let pdamage = (cPOISON_DAMAGE *. 
            (float_of_int starter.attack) /. (float_of_int starter.defense)) in
          let hp_diff = 
            if (int_of_float pdamage) > starter.curr_hp then starter.curr_hp
            else (int_of_float pdamage) in
          Netgraphics.add_update (NegativeEffect(
            starter.species ^ "was hurt by poison!", team, hp_diff));
          Netgraphics.add_update
            (UpdateSteammon(starter.species, starter.curr_hp - hp_diff,
            starter.max_hp, team));
          use_pp (update_hp starter pdamage)
        | _ -> use_pp starter in
    Netgraphics.add_update
      (SetStatusEffects(updated_starter.species, updated_starter.status));
    ((process_mods updated_starter)::(List.tl steammon_lst), inventory) in
  (* processes attack power *)
  let attack_power (attacker: team_data) (defender: team_data) : float = 
      let (atk_steammon, _) = attacker in
      let starter = List.hd atk_steammon in (* active attacking steammon *)
      let (def_steammon,_) = defender in
      let def_steammon = (List.hd def_steammon) in (*active defending steammon *)
      let (def_type1, def_type2) =
        (def_steammon.first_type, def_steammon.second_type) in
      let attack = 
        if (is_special) then float_of_int starter.spl_attack
        else ((float_of_int starter.attack) *. (mod_attack starter)) in
      let crit = if (Random.int 99 < a.crit_chance) then cCRIT_MULTIPLIER else 1. in
      let stab = 
        match starter.first_type, starter.second_type with
        | Some t1, Some t2 -> 
          if (a.element = t1 || a.element = t2) then cSTAB_BONUS else 1.
        | Some t1, None -> if (a.element = t1) then cSTAB_BONUS else 1.
        | _ -> 1. in
      let type_multiplier =
        match def_type1, def_type2 with
  | Some t1, Some t2 -> (weakness a.element t1) *. (weakness a.element t2)
  | Some t1, None -> (weakness a.element t1)
  | _ -> 1. in
      let hit_attack =
        let chance = float_of_int (Random.int 99) in
        match starter.mods.accuracy_mod with
        | -3 -> (cACCURACY_DOWN3 *. (float_of_int a.accuracy)) > chance
        | -2 -> (cACCURACY_DOWN2 *. (float_of_int a.accuracy)) > chance
        | -1 -> (cACCURACY_DOWN1 *. (float_of_int a.accuracy)) > chance
        | 0 -> (float_of_int a.accuracy) > chance
        | 1 -> (cACCURACY_UP1 *. (float_of_int a.accuracy)) > chance
        | 2 -> (cACCURACY_UP2 *. (float_of_int a.accuracy)) > chance
        | 3 -> (cACCURACY_UP3 *. (float_of_int a.accuracy)) > chance
        | _ -> false in
      if not hit_attack then
        Netgraphics.add_update (NegativeEffect("Miss!",
          opposite_color team, 0));
      if (attack_self_if_confused && not (snap_out_of_confused) &&
        List.mem Confused starter.status) ||
        (stuck_if_paralyzed && (List.mem Confused starter.status)) ||
        (not (defrost_if_frozen) && (List.mem Frozen starter.status)) ||
        (not (wake_up_if_asleep) && (List.mem Asleep starter.status)) ||
        (not hit_attack) then 0.
      else
         let defense =
           if is_special then float_of_int def_steammon.spl_defense 
           else (float_of_int def_steammon.defense) *. (mod_defense def_steammon) in
         print_endline ("attack: " ^ (string_of_float attack) ^ " crit: " ^ (string_of_float crit) ^ " stab: " ^ (string_of_float stab) ^ " typemult: " ^ (string_of_float type_multiplier) ^ " defense: " ^ (string_of_float defense));
        (float_of_int a.power) *. attack *. crit *. stab *. type_multiplier /.
        defense in    
  (* processes changes in defender's state *)
  let defender_helper (t_data: team_data) (damage: float): team_data = 
    let (steammon_lst, inventory) = t_data in
    let starter = List.hd steammon_lst in (* active defending steammon *)
    let process_status (s:steammon) = 
      let (status, prob) = a.effect in
      if (Random.int 99) < prob then
        match status with
        | Poisons -> add_status s Poisoned
        | Confuses -> add_status s Confused
        | Sleeps -> add_status s Asleep
        | Paralyzes -> add_status s Paralyzed
        | Freezes -> add_status s Frozen
        | OpponentAttackDown1 -> change_mod s status
        | OpponentDefenseDown1 -> change_mod s status
        | OpponentSpeedDown1 -> change_mod s status
        | OpponentAccuracyDown1 -> change_mod s status
  | _ -> s
      else s in    
    let hp_diff =
      if (int_of_float damage) > starter.curr_hp then starter.curr_hp
      else int_of_float damage in
    let updated_steammon = process_status (update_hp starter damage) in
print_endline ("remaining hp: " ^ (string_of_int updated_steammon.curr_hp));
print_endline ("damage: " ^ (string_of_int hp_diff));
    Netgraphics.add_update (NegativeEffect(updated_steammon.species,
      team, hp_diff));
    Netgraphics.add_update
      (UpdateSteammon(updated_steammon.species, updated_steammon.curr_hp,
      updated_steammon.max_hp, team));
    Netgraphics.add_update
      (SetStatusEffects(updated_steammon.species, updated_steammon.status));
    (updated_steammon::(List.tl steammon_lst), inventory) in
  match team with
  | Red -> set_game_data st 
    ((attacker_helper red_data),
    (defender_helper blue_data (attack_power red_data blue_data)))
  | Blue -> set_game_data st 
    ((defender_helper red_data (attack_power blue_data red_data)),
    (attacker_helper blue_data))


(* Applies an item effect on a target steammon *)
let use_item (st: state) (team: color) (item: item) (target: steammon) : unit = 
print_endline ((color_to_string team) ^  " using " ^ (string_of_item item));
  let ((r_steammon, r_inventory), (b_steammon, b_inventory)) = st.game_data in
  Netgraphics.add_update 
    (Message((color_to_string team) ^ " used " ^ (string_of_item item) ^ "!"));
  let apply (steammon: steammon list) =
    List.fold_left (fun acc x ->
    if x.species = target.species || item = XAttack || item = XDefense 
      || item = XSpeed || item = XAccuracy then begin
      let use_ether (attack: attack) = 
        let pp_rem = 
          match attack with
          {max_pp = max; pp_remaining = pp} -> if pp + 5 >= max then max
            else pp + 5 in
        {name = attack.name;
        element = attack.element;
        max_pp = attack.max_pp;
        pp_remaining = pp_rem;
        power = attack.power;
        accuracy = attack.accuracy;
        crit_chance = attack.crit_chance;
        effect = attack.effect} in
      (* a steammon with updated values after applying item effects *)
      let updated_steammon = 
        {species = target.species;
        curr_hp =
          if item = MaxPotion && target.curr_hp <> 0 then begin
            Netgraphics.add_update (PositiveEffect(target.species,
              team, target.max_hp - target.curr_hp));
            Netgraphics.add_update
              (UpdateSteammon(target.species, target.max_hp,
              target.max_hp, team));
            target.max_hp end
          else if item = Revive then begin
            if target.curr_hp = 0 then begin
              Netgraphics.add_update (PositiveEffect(target.species,
                team, target.max_hp/2));
              Netgraphics.add_update
                (UpdateSteammon(target.species, target.max_hp / 2,
                target.max_hp, team));
              target.max_hp / 2 end
            else target.curr_hp; end (* revive does nothing *)
          else target.curr_hp;
        max_hp = target.max_hp;
        first_type = target.first_type;
        second_type = target.second_type; 
        first_attack = 
          if item = Ether then use_ether target.first_attack
          else target.first_attack;
        second_attack = 
          if item = Ether then use_ether target.second_attack
          else target.second_attack;
        third_attack =
          if item = Ether then use_ether target.third_attack
          else target.third_attack;
        fourth_attack =
          if item = Ether then use_ether target.fourth_attack
          else target.fourth_attack;
        attack = target.attack;
        spl_attack = target.spl_attack;
        defense = target.defense;
        spl_defense = target.spl_defense;
        speed = target.speed;
        status =
          if item = FullHeal || item = Revive then []
          else target.status;
        mods =
        (* applies mod to active steammon even if target is incorrect *)
        if is_active st team x then
    let f (i : int) = if i = 3 then 3 else i + 1 in
          if item = XAttack then 
            {attack_mod = f target.mods.attack_mod;
            speed_mod = target.mods.speed_mod;
            defense_mod = target.mods.defense_mod;
            accuracy_mod = target.mods.accuracy_mod}
          else if item = XDefense then
            {attack_mod = target.mods.attack_mod;
            speed_mod = target.mods.speed_mod;
            defense_mod = f target.mods.defense_mod;
            accuracy_mod = target.mods.accuracy_mod}
          else if item = XSpeed then
            {attack_mod = target.mods.attack_mod;
            speed_mod = f target.mods.speed_mod;
            defense_mod = target.mods.defense_mod;
            accuracy_mod = target.mods.accuracy_mod}
          else if item = XAccuracy then
            {attack_mod = target.mods.attack_mod;
            speed_mod = target.mods.speed_mod;
            defense_mod = target.mods.defense_mod;
            accuracy_mod = f target.mods.accuracy_mod}
          else target.mods
        else target.mods} in
      Netgraphics.add_update
        (SetStatusEffects(updated_steammon.species, updated_steammon.status));
      updated_steammon::acc end
      else x::acc
    ) [] steammon in
  match team with 
  | Red -> 
    if (inventory_contains st Red item) then
      set_game_data st
      ((apply r_steammon, remove_item r_inventory item), (b_steammon, b_inventory))
  | Blue ->
    if (inventory_contains st Red item) then
      set_game_data st
      ((r_steammon, r_inventory), (apply b_steammon, remove_item b_inventory item))
