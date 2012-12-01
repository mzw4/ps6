open Team
open Definitions
open Constants

(* Attention Student:
 * You only need to modify the handle_request function. Do not change its arguments.
 * You should change all of the inside and write many helper functions if you
 * want to have a good bot.
 *)
let _ = Random.self_init () 

let handle_request c r =
  match r with
    | StarterRequest(gs)->
      let((r_steammon, r_inventory), (b_steammon, b_inventory)) = gs in
      let lst = 
        match c with 
  			| Red -> r_steammon
  			| Blue -> b_steammon
      let pick = try List.find(fun x -> x.curr_hp > 0) lst with _ -> (List.hd lst) in
        SelectStarter(pick.species)
    | PickRequest(_, _, _, sp) ->
        let((r_steammon, r_inventory), (b_steammon, b_inventory)) = gs in
        let my_steammon = 
          match c with 
	  | Red -> r_steammon
	  | Blue -> b_steammon in
        let stats_tbl = Hashtbl.create 300 in
        List.fold_left (fun acc x ->
          let stats = (x.max_hp / 4) + x.attack + x.spl_attack + x.defense +
            x.spl_defense + x.speed in
          Hashtbl.add stats_tbl (x.species) (stats, x.first_type, x.second_type)
        ) () sp;
        if Hashtbl.length > 0 then
          (* do not want if already have steammon or
             already have 2 steammon of a type *)
          let still_want steamons n t1 t2 =
            let rating = List.fold_left (fun acc x ->
              if x.species = n then 2
              else if ((x.first_type = t1 || x.second_type = t1) && t1 <> None) ||
                       (x.first_type = t2 || x.second_type = t2) && t2 <> None) then 
                acc + 1
              else acc) 0 steammons in
            (rating < 2) in
          let (name,_,_,_) = 
            Hashtbl.fold (fun k (n, s, t1, t1) (species, stats) -> 
              if s > stats && (still_want my_steammon n t1 t2) then
                (n, s, t1, t2)
              else acc) stats_tbl ("", 0, Normal, Normal) in name
        else failwith "no steammon available to pick!"
    | ActionRequest (gr) ->
        let (red_data, blue_data) = gr in
				
				(* Calculates the strongest attack based on attacker and defender stats, STAB, and weaknesses*)
				(* outputs a the damage done as a ratio to opponent's hp *)
				let attack_power (attacker: steammon) (defender: steammon) (att: attack) : (attack * float) =  
				  let is_special= 
            match att.steamtpye with
            | Electric
            | Fire 
            | Water
            | Psychic
            | Ghost -> true
            | _ -> false
            in
					let mod_attack : float = 
        		match attacker.mods.attack_mod with
        		| -3 -> cATTACK_DOWN3
        		| -2 -> cATTACK_DOWN2
        		| -1 -> cATTACK_DOWN1
        		| 1 -> cATTACK_UP1
        		| 2 -> cATTACK_UP2
        		| 3 -> cATTACK_UP3
        		| _	-> 1.
        		in
        	let mod_defense : float = 
        		match defender.mods.defense_mod with
        		| -3 -> cDEFENSE_DOWN3
        		| -2 -> cDEFENSE_DOWN2
        		| -1 -> cDEFENSE_DOWN1
        		| 1 -> cDEFENSE_UP1
        		| 2 -> cDEFENSE_UP2
        		| 3 -> cDEFENSE_UP3
        		| _	-> 1.
        		in	
					let power = 
						match is_special with
						| true -> (attacker.attack * mod_attack) /. (defender.defense * mod_defense)
						| false -> starter.spl_attack /. defender.spl_defense  
						in
    			(* let crit = a.crit_chance *. cCRIT_MULTIPLIER /. 100. + (100. - a.crit_chance) /. 100. *)
    			let stab = 
      			match defender.first_type with
      			| Some t1 ->
              match defender.second_type with
              | Some t2 -> if (att.steamtype = t1 || att.steamtype = t2) then cSTAB_BONUS else 1.
              | None -> if (att.stamtype = t1) then cSTAB_BONUS else 1.
            | None -> 1.   
            in
          let type_multplier = 
            match starter.first_type with
            |  Some t1 ->
                match starter.second_type with
                | Some t2 -> (weakness att.steamtype t1) *. (weakness att.steamtype t2)
                | None -> weakness att.steamtype t1
            | None -> 1.	
						in
					if (att.pp_remaining = 0) then 0.0
					else att.power *. power *. stab *. type_multiplier (* *. crit *)
					in
				
				(* Finds if an attack can poison opponent *)
				(* Returns the effect chance *)				
				let poison_effect (att: attack) : float option =
					if (fst att.effect) = Poisons then Some (float_of_int (snd att.effect)) /. 100.
					else None
					in
					
				(* Finds if an attack can confuse opponent *)
				(* Returns the effect chance *)				
				let poison_effect (att: attack) : float option =
					if (fst att.effect) = Confuses then Some (float_of_int (snd att.effect)) /. 100.
					else None
					in
							
				(* Finds if an attack can sleep opponent *)
				(* Returns the effect chance *)				
				let sleep_effect (att: attack) : float option =
					if (fst att.effect) = Sleeps then Some (float_of_int (snd att.effect)) /. 100.
					else None
					in
					
				(* Finds if an attack can paralyze opponent *)
				(* Returns the effect chance *)				
				let paralyze_effect (att: attack) : float option =
					if (fst att.effect) = Paralyzes then Some (float_of_int (snd att.effect)) /. 100.
					else None
					in
				
				(* Finds if an attack can freeze opponent *)
				(* Returns the effect chance *)				
				let freeze_effect (att: attack) : float option =
					if (fst att.effect) = Freezes then Some (float_of_int (snd att.effect)) /. 100.
					else None
					in
				
				(* Finds if an attack can increase attacker's attack stat*)
				(* Returns a tuple of the % increase in attack stat and effect chance *)						
				let attack_up (attacker: steammon) (att: attack) : (float * float) option =
					if (fst att.effect) = SelfAttackUp1 then
  					let helper1 =
  						match attacker.mods.attack_mod with
  						| 3 -> 1.
  						| 2 -> cATTACK_UP3 /. cATTACK_UP2
  						| 1 -> cATTACK_UP2 /. cATTACK_UP1
  						| 0 -> cATTACK_UP1
  						| -1 -> 1. /. cATTACK_DOWN1
  						| -2 -> cATTACK_DOWN1 /. cATTACK_DOWN2
  						| -3 -> cATTACK_DOWN2 /. cATTACK_DOWN3
							| _ -> 1.
							in
  					let helper2 = 
  						(float_of_int (snd att.effect)) /. 100.
							in
						Some (helper1, helper2)
					else
						None
					in
					
				(* Finds if an attack can increase attacker's speed stat*)
				(* Returns a tuple of the % increase in speed stat and effect chance *)					
				let speed_up (attacker: steammon) (att: attack): (float * float) option =
					if (fst att.effect) = SelfSpeedUp1 then
  					let helper1 =
  						match attacker.mods.speed_mod with
  						| 3 -> 1.
  						| 2 -> cSPEED_UP3 /. cSPEED_UP2
  						| 1 -> cSPEED_UP2 /. cSPEED_UP1
  						| 0 -> cSPEED_UP1
  						| -1 -> 1. /. cSPEED_DOWN1
  						| -2 -> cSPEED_DOWN1 /. cSPEED_DOWN2
  						| -3 -> cSPEED_DOWN2 /. cSPEED_DOWN3
							| _ -> 1.
							in
  					let helper2 = 
  						(float_of_int (snd att.effect)) /. 100.
							in 
						Some (helper1, helper2)
					else 
						None
  				in															
		
				(* Finds if an attack can increase attacker's defense stat*)
				(* Returns a tuple of the % increase in defense stat and effect chance *)							
				let defense_up (attacker: steammon) (att: attack): (float * float) option =
					if (fst att.effect) = SelfDefenseUp1 then
  					let helper1 =
  						match attacker.mods.defense_mod with
  						| 3 -> 1.
  						| 2 -> cDEFENSE_UP3 /. cDEFENSE_UP2
  						| 1 -> cDEFENSE_UP2 /. cDEFENSE_UP1
  						| 0 -> cDEFENSE_UP1
  						| -1 -> 1. /. cDEFENSE_DOWN1
  						| -2 -> cDEFENSE_DOWN1 /. cDEFENSE_DOWN2
  						| -3 -> cDEFENSE_DOWN2 /. cDEFENSE_DOWN3
							| _ -> 1.
							in
  					let helper2 = 
  						(float_of_int (snd att.effect)) /. 100.
						in
						Some (helper1, helper2)
					else 
						None
					in		
					
				(* Finds if an attack can increase attacker's accuracy stat*)
				(* Returns a tuple of the % increase in accuracy stat and effect chance *)						
				let accuracy_up (attacker: steammon) (att: attack) : (float * float) option =
					if (fst att.effect) = SelfAccuracyUp1 then
  					let helper1 =
  						match attacker.mods.accuracy_mod with
  						| 3 -> 1.
  						| 2 -> cACCURACY_UP3 /. cACCURACY_UP2
  						| 1 -> cACCURACY_UP2 /. cACCURACY_UP1
  						| 0 -> cACCURACY_UP1
  						| -1 -> 1. /. cACCURACY_DOWN1
  						| -2 -> cACCURACY_DOWN1 /. cACCURACY_DOWN2
  						| -3 -> cACCURACY_DOWN2 /. cACCURACY_DOWN3
  						| _ -> 1.
  						in
  					let helper2 = 
  						(float_of_int (snd att.effect)) /. 100.
  						in
  					Some (helper1, helper2)
					else
						None
					in
				
				(* Finds if an attack can decrease defender's attack stat*)
				(* Returns a tuple of the % decrease in attack stat and effect chance *)						
				let attack_down (defender: steammon) (att: attack) : (float * float) option =
					if (fst att.effect) = OpponentAttackDown1 then
  					let helper1 =
  						match defender.mods.attack_mod with
  						| 3 -> cATTACK_UP2 /. cATTACK_UP3 
  						| 2 -> cATTACK_UP1 /. cATTACK_UP2 
  						| 1 -> 1. /. cATTACK_UP1
  						| 0 -> cATTACK_DOWN1
  						| -1 -> cATTACK_DOWN2 /. cATTACK_DOWN1
  						| -2 -> cATTACK_DOWN3 /. cATTACK_DOWN2
  						| -3 -> 1.
							| _ -> 1.
							in
  					let helper2 = 
  						(float_of_int (snd att.effect)) /. 100.
							in
						Some (helper1, helper2)
					else
						None
					in
					
				(* Finds if an attack can decrease defender's speed stat*)
				(* Returns a tuple of the % decrease in speed stat and effect chance *)						
				let speed_down (defender: steammon) (att: attack) : (float * float) option =
					if (fst att.effect) = OpponentSPEEDDown1 then
  					let helper1 =
  						match defender.mods.speed_mod with
  						| 3 -> cSPEED_UP2 /. cSPEED_UP3 
  						| 2 -> cSPEED_UP1 /. cSPEED_UP2 
  						| 1 -> 1. /. cSPEED_UP1
  						| 0 -> cSPEED_DOWN1
  						| -1 -> cSPEED_DOWN2 /. cSPEED_DOWN1
  						| -2 -> cSPEED_DOWN3 /. cSPEED_DOWN2
  						| -3 -> 1.
							| _ -> 1.
							in
  					let helper2 = 
  						(float_of_int (snd att.effect)) /. 100.
							in
						Some (helper1, helper2)
					else
						None
					in
					
				(* Finds if an attack can decrease defender's defense stat*)
				(* Returns a tuple of the % decrease in defense stat and effect chance *)						
				let defense_down (defender: steammon) (att: attack) : (float * float) option =
					if (fst att.effect) = OpponentDefenseDown1 then
  					let helper1 =
  						match defender.mods.defense_mod with
  						| 3 -> cDEFENSE_UP2 /. cDEFENSE_UP3 
  						| 2 -> cDEFENSE_UP1 /. cDEFENSE_UP2 
  						| 1 -> 1. /. cDEFENSE_UP1
  						| 0 -> cDEFENSE_DOWN1
  						| -1 -> cDEFENSE_DOWN2 /. cDEFENSE_DOWN1
  						| -2 -> cDEFENSE_DOWN3 /. cDEFENSE_DOWN2
  						| -3 -> 1.
							| _ -> 1.
							in
  					let helper2 = 
  						(float_of_int (snd att.effect)) /. 100.
							in
						Some (helper1, helper2)
					else
						None
					in
					
				(* Finds if an attack can decrease defender's attack stat*)
				(* Returns a tuple of the % decrease in attack stat and effect chance *)						
				let accuracy_down (defender: steammon) (att: attack) : (float * float) option =
					if (fst att.effect) = OpponentAccuracyDown1 then
  					let helper1 =
  						match defender.mods.accuracy_mod with
  						| 3 -> cACCURACY_UP2 /. cACCURACY_UP3 
  						| 2 -> cACCURACY_UP1 /. cACCURACY_UP2 
  						| 1 -> 1. /. cACCURACY_UP1
  						| 0 -> cACCURACY_DOWN1
  						| -1 -> cACCURACY_DOWN2 /. cACCURACY_DOWN1
  						| -2 -> cACCURACY_DOWN3 /. cACCURACY_DOWN2
  						| -3 -> 1.
							| _ -> 1.
							in
  					let helper2 = 
  						(float_of_int (snd att.effect)) /. 100.
							in
						Some (helper1, helper2)
					else
						None
					in		
				
				let strongest_attack (attacker: steammon) (defender: steammon) : (attack * float) =
					let current = ref attacker.first_attack in
					if (attack_power attacker defender !current) < (attack_power attacker defender attacker.second_attack)
						then current := attacker.second_attack;
					if (attack_power attacker defender !current) < (attack_power attacker defender attacker.third_attack)
						then current := attacker.second_attack;
					if (attack_power attacker defender !current) < (attack_power attacker defender attacker.fourth_attack)
						then current := attacker.second_attack;
					(!current, (attack_power attacker defender !current))	
				let calculate_weights (attacker: steammon) (defender: steammon) (att: attack) : float	=
					let cPOISON_WEIGHT =
																										
																			
        (*
				(match mons with
        | h::t ->
    	    if h.curr_hp < h.max_hp && b > 0 then UseItem (MaxPotion,h.species) else
            if (h.first_attack).pp_remaining >0 then
              let _ = print_endline (h.species ^ "used " ^ ((h.first_attack).name)) in
                UseAttack((h.first_attack).name)
            else if ((h.second_attack).pp_remaining > 0) then
              let _ = print_endline (h.species ^ "used " ^ ((h.second_attack).name)) in
                UseAttack((h.second_attack).name)
            else if ((h.third_attack).pp_remaining > 0) then
              let _ = print_endline (h.species ^ "used " ^ ((h.third_attack).name)) in
                UseAttack((h.third_attack).name)
            else
              let _ = print_endline (h.species ^ "used " ^ ((h.fourth_attack).name)) in
                UseAttack((h.fourth_attack).name)
								
        | _ -> failwith "WHAT IN THE NAME OF ZARDOZ HAPPENED HERE")
				*)
    | PickInventoryRequest (gr) -> 
      let cash = ref cINITIAL_CASH in
      let maxPotionCash = cash / 2 in
      let reviveCash = cash / 3 in
      let(r_data, b_data) = gs in
      let (steammon, inventory) = 
        match c with 
	| Red -> r_data
	| Blue -> b_data in
      let ethers = 0 in
      let max_potions = (cash / 2) / cCOST_MAXPOTION in
      cash := cash - (max_potions * cCOST_MAXPOTION);
      let revives = (cash / 3) / cCOST_REVIVE in
      cash := cash - (revives * cCOST_REVIVE);
      let full_heals = cash / cCOST_FULLHEAL in 
      cash := cash - (full_heals * cCOST_FULLHEAL);
      let xattacks = 0 in
      let xdefenses = 0 in
      let xaccuracies = 0 in
      let xspeeds = 0 in
      PickInventory(
	[ethers; max_potions; revives; full_heals;
	xattacks; xdefenses; xaccuracies; xspeeds])
let () = run_bot handle_request
