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
      let steammon_tbl = Hashtbl.create 300 in
      List.fold_left (fun acc x ->
        let statsum = (x.maxHP / 4) + x.attack + x.spl_attack + x.defense +
          x.spl_defense + x.speed in
        Hashtbl.add (x.species) statsum
      ) () sp;
      if Hashtbl.length > 0 then
        let (name, statsum) = 
          Hashtbl.fold (fun k (n, s) (species, stats) -> 
            if s > stats then (n, s) else acc) steammon_tbl ("", 0) in name
      else failwith "no steammon available to pick!"
    | ActionRequest (gr) ->
        let (red_data, blue_data) = gr in
				let strongest_attack (attacker: steammon) (defender: steammon) : (attack * float) = 
					let helper (att: attack) : float = 
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
						let attack_power = 
							match is_special with
							| true -> (attacker.attack * mod_attack) /. (defender.defense * mod_defense)
							| false -> starter.spl_attack /. defender.spl_defense  
							in
      			(* let crit = if (Random.int 99 < a.crit_chance) then cCRIT_MULTIPLIER else 1 in *)
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
						else attack_power *. stab *. type_multiplier
						in
					let best_attack = ref attacker.first_attack in
					if ((helper !best_attack) < (helper attacker.second_attack)) 
						then best_attack := attacker.second_attack;
					else if ((helper !best_attack) < (helper attacker.third_attack))
						then best_attack := attacker.third_attack;
					else if ((helper !best_attack) < (helper attacker.fourth_attack))
						then best_attack := attacker.fourth_attack;
					(!best_attack, ((helper (!best_attack)) /. defender.current_hp))
					in	
        let (steammon, inventory) = team in
				
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
