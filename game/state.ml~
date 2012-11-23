open Definitions
open Util
open Constants
open Netgraphics

type state = {
  mutable current_phase: phase;
  mutable game_data: game_status_data;
  mutable undrafted_steammon: steammon list option;
  }
  
type phase = 
  | Init
  | SelectSteammon
  | SelectItems
  | Battle

let create : state = 
  {current_phase = Init;
  game_data = (([], [0;0;0;0;0;0;0;0]), ([], [0;0;0;0;0;0;0;0]));
  undrafted_steammon = None;
  }
  
let set_phase (st: state) (ph: phase) : unit =
  st.phase <- ph;

let set_game_data (st: state) (data: game_status_data) : unit =
  st.game_data <- data;	
	
(* Adds a steammon to a team's steammon list. *)
(* Throws an exception if that steammon is not in the draftable list*)
let add_steammon (st: state) (team: color) (st: steammon) : unit =
  let (red_data, blue_data) = st.game_data in
  let helper (t_data: team_data) : team_data =
    match st.undrafted_steammon with
    | Some available->
      if (List.mem st available) then 
        let (lst, inventory) = t_data in
        available <- Some (List.fold_left (fun a x -> if (x = st) then a else x::a) [] available) in
        (st::lst, inventory)
      else 
        failwith "Steammon is not able to be selected"      
    | None -> failwith "There are no steammon able to be picked"
  in
  match team with
  | Red -> set_game_data st ((helper red_data), blue_data)
  | Blue -> set_game_data st (red_data, (helper blue_data)

(* Switches a steammon so it appears at the head of a the steammon list. *)
(* Throws an exception if that steammon is not in the list. *)
let switch_steammon (st: state) (team: color) (st: steammon) : unit =
  let (red_data, blue_data) = st.game_data in
  let helper (t_data: team_data) : team_data =
    let (lst, inventory) = t_data in
    if (List.mem st lst) then
      let tl = List.fold_left (fun a x -> if (x = st) then a else x::a) [] lst in
      (st::tl, inventory)
    else
      failwith "Steammon selected is not in the team."
  in  
  match team with 
  | Red -> set_game_data st ((helper red_data), blue_data);
  | Blue -> set_game_data st (red_data, (helper blue_data);

(* Sets a player's inventory to the specified one *)
let set_inventory (st: state) (team: color) (inventory: int list) = 
  let ((r_steammon, r_inventory), (b_steammon, b_inventory)) = st.game_data in
  if team = Red then
    set_game_data st ((r_steammon, inventory), (b_steammon, b_inventory))
  else 
    set_game_data st ((r_steammon, r_inventory), (b_steammon, inventory))

(* Adds an item to a team's inventory *)  
let add_item (st: state) (team: color) (i: item) : unit =
  let (red_data, blue_data) = st.game_data in
  let helper (t_data: team_data) : team_data =
    let (lst, inventory) = t_data in
    let [ethers; maxPotions; revives; fullHeals; xAttacks; xDefenses; xSpeeds; xAccuracies] = inventory in
    let newInventory = 
    match i with
      | Ether -> [(ethers + 1) ; maxPotions; revives; fullHeals; xAttacks; xDefenses; xSpeeds; xAccuracies]
      | MaxPotion -> [ethers; (maxPotions + 1); revives; fullHeals; xAttacks; xDefenses; xSpeeds; xAccuracies]
      | Revive -> [ethers; maxPotions; (revives + 1); fullHeals; xAttacks; xDefenses; xSpeeds; xAccuracies]
      | FullHeal -> [ethers; maxPotions; revives; (fullHeals + 1); xAttacks; xDefenses; xSpeeds; xAccuracies]
      | XAttack -> [ethers; maxPotions; revives; fullHeals; (xAttacks + 1); xDefenses; xSpeeds; xAccuracies]
      | XDefense -> [ethers; maxPotions; revives; fullHeals; xAttacks; (xDefenses + 1); xSpeeds; xAccuracies]
      | XSpeed -> [ethers; maxPotions; revives; fullHeals; xAttacks; xDefenses; (xSpeeds + 1); xAccuracies]
      | XAccuracy -> [ethers; maxPotions; revives; fullHeals; xAttacks; xDefenses; xSpeeds; (xAccuracies + 1)]
    in
    (lst, newInventory)
  in
  match team with
  | Red -> set_game_data st ((helper red_data), blue_data)
  | Blue -> set_game_data st (red_data, (helper blue_data)

    
(* Removes an item to a team's inventory. If there are none of that item in the inventory, throws an exception *)  
let remove_item (st: state) (team: color) (i: item) : unit =
  let (red_data, blue_data) = st.game_data in
  let helper (t_data: team_data) : team_data =
    let (lst, inventory) = t_data in
    let [ethers; maxPotions; revives; fullHeals; xAttacks; xDefenses; xSpeeds; xAccuracies] = inventory in
    let f (i: int) : int =
      if (i > 0) then i - 1
      else failwith "There are none of that item left"
    let newInventory = 
    match i with
      | Ether -> [(f ethers) ; maxPotions; revives; fullHeals; xAttacks; xDefenses; xSpeeds; xAccuracies]
      | MaxPotion -> [ethers; (f maxPotions); revives; fullHeals; xAttacks; xDefenses; xSpeeds; xAccuracies]
      | Revive -> [ethers; maxPotions; (f revives); fullHeals; xAttacks; xDefenses; xSpeeds; xAccuracies]
      | FullHeal -> [ethers; maxPotions; revives; (f fullHeals); xAttacks; xDefenses; xSpeeds; xAccuracies]
      | XAttack -> [ethers; maxPotions; revives; fullHeals; (f xAttacks); xDefenses; xSpeeds; xAccuracies]
      | XDefense -> [ethers; maxPotions; revives; fullHeals; xAttacks; (f xDefenses); xSpeeds; xAccuracies]
      | XSpeed -> [ethers; maxPotions; revives; fullHeals; xAttacks; xDefenses; (f xSpeeds); xAccuracies]
      | XAccuracy -> [ethers; maxPotions; revives; fullHeals; xAttacks; xDefenses; xSpeeds; (f xAccuracies)]
    in
    (lst, newInventory)
  in
  match team with
  | Red -> set_game_data st ((helper red_data), blue_data)
  | Blue -> set_game_data st (red_data, (helper blue_data)


(* Processes an attack and calculates the effects *)
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
			else {name = a.name ; element = a.element ; max_pp = a.max_pp; pp_remaining = (a.pp_remaining - 1); 
			power = a.power; accuracy = a.accuracy; crit_chance = a.crit_chance; effect = a.effect} in		
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
			|	Some t1 ->
					match starter.second_type with
					|	Some t2 -> if (a.steamtype = t1 || a.steamtype = t2) then cSTAB_BONUS else 1
					| None -> if (a.stamtype = t1) then cSTAB_BONUS else 1
			| None -> 1 	
			in
		let type_multpipler = 
			match starter.first_type with
			|	Some t1 ->
					match starter.second_type with
					|	Some t2 -> (weakness a.steamtype t1) *. (weakness a.steamtype t2)
					| None -> weakness a.steamtype t1
			| None -> 1 	
			in
		float_of_int (a.power * starter.attack * crit * stab) *. type_multiplier
	in
  let defender_helper (t_data: team_data) (f: float): team_data = 
    let (lst, inventory) = t_data in
    let starter = List.hd lst in
  	
  in
  match team with
  | Red -> set_game_data st 
	((attacker_helper red_data), (defender_helper blue_data (attack_power red_data)))
  | Blue -> set_game_data st 
	((defender_helper red_data (attack_power blue_data)), (attacker_helper blue_data))
  
  
