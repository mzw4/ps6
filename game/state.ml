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
  let rmons = List.fold_left (fun acc x -> acc ^ " " ^(x.species)) "" r_steammon in
  let bmons = List.fold_left (fun acc x -> acc ^" "^ (x.species)) "" b_steammon in
  print_endline ("Red: " ^ rmons);
  print_endline ("Blue: " ^ bmons)


(* Indicates if the steammon has already been drafted *)
let already_selected (st: state) (steammon: steammon) : bool = 
  let ((r_steammon, r_inventory), (b_steammon, b_inventory)) = st.game_data in
  List.fold_left2 (fun acc r b ->
    if r.species = steammon.species || b.species = steammon.species then true
    else acc) false r_steammon b_steammon

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
  let r_speed = (List.hd r_steammon).speed in
  let b_speed = (List.hd b_steammon).speed
  if r_speed > b_speed then Red
  else if b_speed > r_speed then Blue
  else if Random.float 1. > 0.5 then Red
  else Blue

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
    List.fold_left (fun acc s ->
      if s.curr_hp <> 0 then false
      else acc) true steammon in
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
let switch_steammon (st: state) (team: color) (steammon: steammon) : unit =
  let (red_data, blue_data) = st.game_data in
  let helper (t_data: team_data) : team_data =
    let (lst, inventory) = t_data in
    if (List.mem steammon lst) then
      let tl = List.fold_left (fun a x -> if (x = steammon) then a else x::a) [] lst in
      (steammon::tl, inventory)
    else
      failwith "Steammon selected is not in the team."
  in  
  match team with 
  | Red -> set_game_data st ((helper red_data), blue_data)
  | Blue -> set_game_data st (red_data, (helper blue_data))

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

    
(* Removes an item to a team's inventory. If there are none of that item in the inventory, throws an exception *)  
let remove_item (st: state) (team: color) (i: item) : unit =
  let (red_data, blue_data) = st.game_data in
  let helper (t_data: team_data) : team_data =
    let (lst, inventory) = t_data in
    let newInventory = 
      match inventory with 
      | [ethers; maxPotions; revives; fullHeals; xAttacks; xDefenses;
        xSpeeds; xAccuracies] ->
        let f (i: int) : int =
          if (i > 0) then i - 1
          else failwith "There are none of that item left" in
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
      | _ -> failwith "invalid inventory" in
    (lst, newInventory) in
  match team with
  | Red -> set_game_data st ((helper red_data), blue_data)
  | Blue -> set_game_data st (red_data, (helper blue_data))

(* Processes an attack and calculates the effects *)
let attack (st: state) (team: color) (a: attack) : unit = failwith "used attack ogm"
 let attack (st: state) (team: color) (a: attack) : unit = 
  let (red_data, blue_data) = st.game_data in
  let defrost_if_frozen = (Random.int 99) < cDEFROST_CHANCE in
  let wake_up_if_asleep = (Random.int 99) < cWAKE_UP_CHANCE in
  let attack_self_if_confused = (Random.int 99) < cSELF_ATTACK_CHANCE in
  let snap_out_if_confused = (Random.int 99) < cSNAP_OUT_OF_CONFUSION in

  let change_status (s: steammon) (stat: status) : steammon = 
    {species = s.species; 
    curr_hp = s.curr_hp; 
    max_hp = s.max_hp; 
    first_type = s.first_type; 
    second_type = s.second_type; 
    first_attack = s.first_attack; 
    second_attack = s.second_attack, 
    third_attack = s.third_attack; 
    fourth_attack = s.fourth_attack; 
    attack = s.attack; 
    spl_attack = s.spl_attack; 
    defense = s.defense; 
    spl_defense = s.spl_defense; 
    speed = s.speed; 
    status = stat; 
    mods = s.mods}
    in
  let attacker_helper (t_data: team_data) : team_data = 
    let (lst, inventory) = t_data in
    let starter = List.hd lst in
    let f (a: attack) : attack = 
      if (a.pp_remaining <= 0) then failwith "No PP left for that attack" 
      else {name = a.name ; element = a.element ; max_pp = a.max_pp; 
      pp_remaining = (a.pp_remaining - 1); power = a.power; accuracy = 
        a.accuracy; crit_chance = a.crit_chance; effect = a.effect} in    
    let use_attack (s: steammon) : steammon = 
      match a.name with
      | s.first_attack.name -> 
        {species = s.species; 
        curr_hp = s.curr_hp; 
        max_hp = s.max_hp; 
        first_type = f(s.first_type); 
        second_type = s.second_type; 
        first_attack = s.first_attack; 
        second_attack = s.second_attack, 
        third_attack = s.third_attack; 
        fourth_attack = s.fourth_attack; 
        attack = s.attack; 
        spl_attack = s.spl_attack; 
        defense = s.defense; 
        spl_defense = s.spl_defense; 
        speed = s.speed; 
        status = s.status; 
        mods = s.mods}
      | s.second_attack.name ->  
        {species = s.species; 
        curr_hp = s.curr_hp; 
        max_hp = s.max_hp; 
        first_type = s.first_type; 
        second_type = f(s.second_type); 
        first_attack = s.first_attack; 
        second_attack = s.second_attack, 
        third_attack = s.third_attack; 
        fourth_attack = s.fourth_attack; 
        attack = s.attack; 
        spl_attack = s.spl_attack; 
        defense = s.defense; 
        spl_defense = s.spl_defense; 
        speed = s.speed; 
        status = s.status; 
        mods = s.mods}
      | s.third_attack.name ->
        {species = s.species; 
        curr_hp = s.curr_hp; 
        max_hp = s.max_hp; 
        first_type = s.first_type; 
        second_type = s.second_type; 
        first_attack = s.first_attack; 
        second_attack = s.second_attack, 
        third_attack = f(s.third_attack); 
        fourth_attack = s.fourth_attack; 
        attack = s.attack; 
        spl_attack = s.spl_attack; 
        defense = s.defense; 
        spl_defense = s.spl_defense; 
        speed = s.speed; 
        status = s.status; 
        mods = s.mods}
      | s.fourth_attack.name -> 
        {species = s.species; 
        curr_hp = s.curr_hp; 
        max_hp = s.max_hp; 
        first_type = s.first_type; 
        second_type = s.second_type; 
        first_attack = s.first_attack; 
        second_attack = s.second_attack, 
        third_attack = s.third_attack; 
        fourth_attack = f(s.fourth_attack); 
        attack = s.attack; 
        spl_attack = s.spl_attack; 
        defense = s.defense; 
        spl_defense = s.spl_defense; 
        speed = s.speed; 
        status = s.status; 
        mods = s.mods}
      | _ -> failwith "Steammon does not have that attack"
    in
    let confused (s: steammon) : steammon = 
      
    let updated_starter =
      if (List.mem Confused starter.status) then 
        begin
        if (List.mem Frozen starter.status) then
          if (defrost_if_frozen) then
            change_status starter confused
        end    
      else begin
        if (List.mem Frozen starter.status) then
          
        end
    in
    ((updated_starter)::(List.tl lst), inventory)    
  in
  (* does not account for mods yet *)
  let attack_power (t_data: team_data) : float = 
    let (lst, inventory) = t_data in
    let starter = List.hd lst in
    let crit = if (Random.int 99 < a.crit_chance) then cCRIT_MULTIPLIER else 1 in
    let stab = 
      match starter.first_type with
      |  Some t1 ->
          match starter.second_type with
          |  Some t2 -> if (a.steamtype = t1 || a.steamtype = t2) then cSTAB_BONUS else 1
          | None -> if (a.stamtype = t1) then cSTAB_BONUS else 1
      | None -> 1   
      in
    let type_multpipler = 
      match starter.first_type with
      |  Some t1 ->
          match starter.second_type with
          |  Some t2 -> (weakness a.steamtype t1) *. (weakness a.steamtype t2)
          | None -> weakness a.steamtype t1
      | None -> 1   
      in
    float_of_int (a.power * starter.attack * crit * stab) *. type_multiplier
  in
  let defender_helper (t_data: team_data) (f: float): team_data = 
    let (lst, inventory) = t_data in
    let starter = List.hd lst in
    let new_hp = if (starter.current_hp < ((int_of_float) f) then 0 
      else (starter.current_hp - ((int_of_float) f)) in
    let updated_steammon = 
      {species = s.species; 
        curr_hp = new_hp; 
        max_hp = s.max_hp; 
        first_type = s.first_type; 
        second_type = s.second_type; 
        first_attack = s.first_attack; 
        second_attack = s.second_attack, 
        third_attack = s.third_attack; 
        fourth_attack = s.fourth_attack; 
        attack = s.attack; 
        spl_attack = s.spl_attack; 
        defense = s.defense; 
        spl_defense = s.spl_defense; 
        speed = s.speed; 
        status = s.status; 
        mods = s.mods}
    in
    (updated_steammon)::(List.tl lst), inventory)
  in
  match team with
  | Red -> set_game_data st 
  ((attacker_helper red_data), (defender_helper blue_data (attack_power red_data)))
  | Blue -> set_game_data st 
  ((defender_helper red_data (attack_power blue_data)), (attacker_helper blue_data))

(* Applies an item effect on a target steammon *)
let use_item (st: state) (team: color) (item: item) (target: steammon) : unit = 
  let ((r_steammon, r_inventory), (b_steammon, b_inventory)) = st.game_data in
  let apply (steammon: steammon list) =
    List.fold_left (fun acc x ->
    if x.species = target.species || item = XAttack || item = XDefense 
      || item = XSpeed || item = XAccuracy then begin
      let pp_rem (attack: attack) = 
        match attack with
        {max_pp = max; pp_remaining = pp} -> if pp + 5 >= max then max
        else pp + 5 in
      (* a steammon with updated values after applying item effects *)
      let updated_steammon = 
        {species = target.species;
        curr_hp =
          if item = MaxPotion then target.max_hp
          else if item = Revive then
            if target.curr_hp = 0 then target.max_hp / 2
            else failwith "cannot use revive on a non-fainted steammon"
          else target.curr_hp;
        max_hp = target.max_hp;
        first_type = target.first_type;
        second_type = target.second_type; 
        first_attack = 
          if item = Ether then
            {name = target.first_attack.name;
            element = target.first_attack.element;
            max_pp = target.first_attack.max_pp;
            pp_remaining = pp_rem (target.first_attack);
            power = target.first_attack.power;
            accuracy = target.first_attack.accuracy;
            crit_chance = target.first_attack.crit_chance;
            effect = target.first_attack.effect}
          else target.first_attack;
        second_attack = 
          if item = Ether then
            {name = target.second_attack.name;
            element = target.second_attack.element;
            max_pp = target.second_attack.max_pp;
            pp_remaining = pp_rem (target.second_attack);
            power = target.second_attack.power;
            accuracy = target.second_attack.accuracy;
            crit_chance = target.second_attack.crit_chance;
            effect = target.second_attack.effect}
          else target.second_attack;
        third_attack =
          if item = Ether then
            {name = target.third_attack.name;
            element = target.third_attack.element;
            max_pp = target.third_attack.max_pp;
            pp_remaining = pp_rem (target.third_attack);
            power = target.third_attack.power;
            accuracy = target.third_attack.accuracy;
            crit_chance = target.third_attack.crit_chance;
            effect = target.third_attack.effect}
          else target.third_attack;
        fourth_attack =
          if item = Ether then
            {name = target.fourth_attack.name;
            element = target.fourth_attack.element;
            max_pp = target.fourth_attack.max_pp;
            pp_remaining = pp_rem (target.fourth_attack);
            power = target.fourth_attack.power;
            accuracy = target.fourth_attack.accuracy;
            crit_chance = target.fourth_attack.crit_chance;
            effect = target.fourth_attack.effect}
          else target.fourth_attack;
        attack = target.attack;
        spl_attack = target.spl_attack;
        defense = target.defense;
        spl_defense = target.spl_defense;
        speed = target.speed;
        status = if item = FullHeal then [] else target.status;
        mods =
        (* applies mod to active steammon even if target is incorrect *)
        if is_active st team x then
          if item = XAttack then 
            {attack_mod = target.mods.attack_mod + 1;
            speed_mod = target.mods.speed_mod;
            defense_mod = target.mods.defense_mod;
            accuracy_mod = target.mods.accuracy_mod}
          else if item = XDefense then
            {attack_mod = target.mods.attack_mod;
            speed_mod = target.mods.speed_mod;
            defense_mod = target.mods.defense_mod + 1;
            accuracy_mod = target.mods.accuracy_mod}
          else if item = XSpeed then
            {attack_mod = target.mods.attack_mod;
            speed_mod = target.mods.speed_mod + 1;
            defense_mod = target.mods.defense_mod;
            accuracy_mod = target.mods.accuracy_mod}
          else if item = XAccuracy then
            {attack_mod = target.mods.attack_mod;
            speed_mod = target.mods.speed_mod;
            defense_mod = target.mods.defense_mod;
            accuracy_mod = target.mods.accuracy_mod + 1}
          else target.mods
        else target.mods} in
      updated_steammon::acc end
      else x::acc
    ) [] steammon in
  if inventory_contains st Red item then 
    (remove_item st Red item;
    match team with 
    | Red -> 
      set_game_data st
      ((apply r_steammon, r_inventory), (b_steammon, b_inventory))
    | Blue ->
      set_game_data st
      ((r_steammon, r_inventory), (apply b_steammon, b_inventory)))
  else failwith "You ran out of that item"
