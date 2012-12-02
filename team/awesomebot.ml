open Team
open Definitions
open Constants
open Util

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
      in
      let pick = try List.find(fun x -> x.curr_hp > 0) lst with _ -> (List.hd lst) in
        SelectStarter(pick.species)
    | PickRequest(_, gs, _, sp) ->
        let((r_steammon, r_inventory), (b_steammon, b_inventory)) = gs in
        let my_steammon = 
          match c with 
          | Red -> r_steammon
          | Blue -> b_steammon 
          in
        let stats_tbl = Hashtbl.create 300 in
        List.fold_left (fun acc x ->
          let stats = (x.max_hp / 4) + x.attack + x.spl_attack + x.defense +
            x.spl_defense + x.speed in
          Hashtbl.add stats_tbl (x.species)
            (x.species, stats, x.first_type, x.second_type)
        ) () sp;
        if Hashtbl.length stats_tbl > 0 then
          (* do not want if already have steammon or
             already have 2 steammon of a type *)
          let still_want steammons n t1 t2 =
            let available lst =
              not (List.exists (fun x -> x.species = n) lst) in
            let rating = List.fold_left (fun acc x ->
              if x.species = n then 2
              else if ((x.first_type = t1 || x.second_type = t1) && t1 <> None) ||
                       ((x.first_type = t2 || x.second_type = t2) && t2 <> None) then 
                acc + 1
              else acc) 0 steammons in
              available r_steammon && available b_steammon && (rating < 2) in
          let (name,_,_,_) = 
            Hashtbl.fold (fun k (n, s, t1, t2) (species, stats, type1, type2) -> 
              if s > stats && (still_want my_steammon n t1 t2) then
                (n, s, t1, t2)
              else (species, stats, type1, type2))
                stats_tbl ("", 0, Some Normal, None) in
          PickSteammon name
        else failwith "no steammon available to pick!"
    | ActionRequest (gr) ->
        let (red_data, blue_data) = gr in      
        
        (* returns if an attack is a special attack or physical attack *)
        let is_special (att: attack) : bool= 
            match att.element with
            | Electric
            | Fire 
            | Water
            | Psychic
            | Ghost -> true
            | _ -> false
            in
            
        (* Calculates the strongest attack based on attacker and defender stats, STAB, and weaknesses*)
        (* outputs a the damage done as a ratio to opponent's hp *)
        let attack_power (attacker: steammon) (defender: steammon) (att: attack) : float =  
          let mod_attack = 
            match attacker.mods.attack_mod with
            | -3 -> cATTACK_DOWN3
            | -2 -> cATTACK_DOWN2
            | -1 -> cATTACK_DOWN1
            | 1 -> cATTACK_UP1
            | 2 -> cATTACK_UP2
            | 3 -> cATTACK_UP3
            | _  -> 1.
            in
          let mod_defense : float = 
            match defender.mods.defense_mod with
            | -3 -> cDEFENSE_DOWN3
            | -2 -> cDEFENSE_DOWN2
            | -1 -> cDEFENSE_DOWN1
            | 1 -> cDEFENSE_UP1
            | 2 -> cDEFENSE_UP2
            | 3 -> cDEFENSE_UP3
            | _  -> 1.
            in  
          let power = 
            match is_special att with
            | true -> (float_of_int attacker.attack *. mod_attack) /. (float_of_int defender.defense *. mod_defense)
            | false -> float_of_int attacker.spl_attack /. float_of_int defender.spl_defense  
            in
          (* let crit = a.crit_chance *. cCRIT_MULTIPLIER /. 100. + (100. - a.crit_chance) /. 100. *)
          let stab = 
            match defender.first_type with
            | Some t1 -> begin
              match defender.second_type with
              | Some t2 -> if (att.element = t1 || att.element = t2) then cSTAB_BONUS else 1.
              | None -> if (att.element = t1) then cSTAB_BONUS else 1.
              end
            | None -> 1.   
            in
          let type_multiplier = 
            match attacker.first_type with
            |  Some t1 -> begin
                match attacker.second_type with
                | Some t2 -> (weakness att.element t1) *. (weakness att.element t2)
                | None -> weakness att.element t1
                end
            | None -> 1.  
            in
          if (att.pp_remaining = 0) then 0.0
          else float_of_int att.power *. power *. stab *. type_multiplier /. float_of_int defender.curr_hp
          in
        
        (* Finds if an attack can poison opponent *)
        (* Returns the effect chance *)        
        let poison_effect (att: attack) : float =
          if (fst att.effect) = Poisons then (float_of_int (snd att.effect)) /. 100.
          else 0.
          in
          
        (* Finds if an attack can confuse opponent *)
        (* Returns the effect chance *)        
        let confused_effect (att: attack) : float =
          if (fst att.effect) = Confuses then (float_of_int (snd att.effect)) /. 100.
          else 0.
          in
              
        (* Finds if an attack can sleep opponent *)
        (* Returns the effect chance *)        
        let sleep_effect (att: attack) : float =
          if (fst att.effect) = Sleeps then (float_of_int (snd att.effect)) /. 100.
          else 0.
          in
          
        (* Finds if an attack can paralyze opponent *)
        (* Returns the effect chance *)        
        let paralyze_effect (att: attack) : float =
          if (fst att.effect) = Paralyzes then (float_of_int (snd att.effect)) /. 100.
          else 0.
          in
        
        (* Finds if an attack can freeze opponent *)
        (* Returns the effect chance *)        
        let freeze_effect (att: attack) : float =
          if (fst att.effect) = Freezes then (float_of_int (snd att.effect)) /. 100.
          else 0.
          in
        
        (* Finds if an attack can increase attacker's attack stat*)
        (* Returns a tuple of the % increase in attack stat and effect chance *)            
        let attack_up (attacker: steammon) (att: attack) : (float * float) =
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
            if (fst att.effect) = SelfAttackUp1 then (float_of_int (snd att.effect)) /. 100. else 0.
            in
           (helper1, helper2)
          in
          
        (* Finds if an attack can increase attacker's speed stat*)
        (* Returns a tuple of the % increase in speed stat and effect chance *)          
        let speed_up (attacker: steammon) (att: attack): (float * float) =
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
            if (fst att.effect) = SelfSpeedUp1 then (float_of_int (snd att.effect)) /. 100. else 0.
            in 
            (helper1, helper2)
          in                              
    
        (* Finds if an attack can increase attacker's defense stat*)
        (* Returns a tuple of the % increase in defense stat and effect chance *)              
        let defense_up (attacker: steammon) (att: attack): (float * float) =
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
            if (fst att.effect) = SelfDefenseUp1 then (float_of_int (snd att.effect)) /. 100. else 0.
            in
          (helper1, helper2)
          in    
          
        (* Finds if an attack can increase attacker's accuracy stat*)
        (* Returns a tuple of the % increase in accuracy stat and effect chance *)            
        let accuracy_up (attacker: steammon) (att: attack) : (float * float) =
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
            if (fst att.effect) = SelfAccuracyUp1 then (float_of_int (snd att.effect)) /. 100. else 0.
            in
          (helper1, helper2)
          in
        
        (* Finds if an attack can decrease defender's attack stat*)
        (* Returns a tuple of the % decrease in attack stat and effect chance *)            
        let attack_down (defender: steammon) (att: attack) : (float * float) =
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
            if (fst att.effect) = OpponentAttackDown1 then (float_of_int (snd att.effect)) /. 100. else 0.
            in
          (helper1, helper2)
          in
          
        (* Finds if an attack can decrease defender's speed stat*)
        (* Returns a tuple of the % decrease in speed stat and effect chance *)            
        let speed_down (defender: steammon) (att: attack) : (float * float) =
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
            if (fst att.effect) = OpponentSpeedDown1 then (float_of_int (snd att.effect)) /. 100. else 0.
            in
          (helper1, helper2)
          in
          
        (* Finds if an attack can decrease defender's defense stat*)
        (* Returns a tuple of the % decrease in defense stat and effect chance *)            
        let defense_down (defender: steammon) (att: attack) : (float * float) =
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
            if (fst att.effect) = OpponentDefenseDown1 then (float_of_int (snd att.effect)) /. 100. else 0.
            in
          (helper1, helper2)
          in
          
        (* Finds if an attack can decrease defender's attack stat*)
        (* Returns a tuple of the % decrease in attack stat and effect chance *)            
        let accuracy_down (defender: steammon) (att: attack) : (float * float) =
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
            if (fst att.effect) = OpponentAccuracyDown1 then (float_of_int (snd att.effect)) /. 100. else 0.
            in
          (helper1, helper2)
          in    
        
        (* calculates the strongest attack an attacker can use *)
        (* returns a tuple of the attack and the ratio of the attack to the defender's current hp *)      
        let strongest_attack (attacker: steammon) (defender: steammon) : (attack * float) =
          let current = ref attacker.first_attack in
          if (attack_power attacker defender !current) < (attack_power attacker defender attacker.second_attack)
            then current := attacker.second_attack;
          if (attack_power attacker defender !current) < (attack_power attacker defender attacker.third_attack)
            then current := attacker.second_attack;
          if (attack_power attacker defender !current) < (attack_power attacker defender attacker.fourth_attack)
            then current := attacker.second_attack;
          (!current, (attack_power attacker defender !current))  
          in
          
        (* calculates the optimal attack in a given situation *)
        let calculate_weights (attacker: steammon) (defender: steammon) (att: attack) : float  = 
          let has_status = (List.mem Poisoned defender.status) || (List.mem Paralyzed defender.status) ||
            (List.mem Asleep defender.status) || (List.mem Frozen defender.status) in
          let is_poisoned = List.mem Poisoned defender.status in
          let is_confused = List.mem Confused defender.status in
          let is_paralyzed = List.mem Paralyzed defender.status in
          let is_asleep = List.mem Asleep defender.status in
          let is_frozen = List.mem Frozen defender.status in
          
          let speed_ratio = 
            let mod_speed (s: steammon) : float =
              match s.mods.speed_mod with
              | 3 -> cSPEED_UP3
              | 2 -> cSPEED_UP2
              | 1 -> cSPEED_UP1
              | -1 -> cSPEED_DOWN1
              | -2 -> cSPEED_DOWN2
              | -3 -> cSPEED_DOWN3
              | _ -> 1.
              in
            let speed = (float_of_int attacker.speed *. (mod_speed attacker)) /. 
              (float_of_int defender.speed *. (mod_speed defender)) in
            if (List.mem Paralyzed attacker.status) then
              if (is_paralyzed) then speed
              else speed /. float_of_int cPARALYSIS_SLOW
            else
              if (is_paralyzed) then 2. *. speed
              else speed
            in            
          let poison_weight (chance: float) : float = 
            if (has_status) then 0. 
            else (cPOISON_DAMAGE /. (snd (strongest_attack attacker defender))) *. chance
            in
          let confuse_weight (chance: float) : float =
            if (is_confused || is_frozen || is_asleep) then 0.
            else ((100. /. float_of_int cSNAP_OUT_OF_CONFUSION) -. 1.) /. 
              ((100. /. float_of_int cSELF_ATTACK_CHANCE) +. 1.) *. chance
            in
          let paralysis_weight (chance: float) : float = 
            if (has_status) then 0.
            else if ((speed_ratio <= 1.) && (speed_ratio >= (1. /. float_of_int cPARALYSIS_SLOW))) then chance
            else (snd (strongest_attack attacker defender)) *. 1. /. ((100. /. float_of_int cPARALYSIS_CHANCE) +. 1.)  *. chance
            in
          let sleep_weight (chance: float) : float =
            if (has_status) then 0.
            else ((100. /. float_of_int cWAKE_UP_CHANCE) -. 1.) *. chance
            in 
          let freeze_weight (chance: float) : float =
            if (has_status) then 0.
            else ((100. /. float_of_int cWAKE_UP_CHANCE) -. 1.) *. chance
            in
          let att_up_weight ((increase, chance): float * float) : float =
            let a = (strongest_attack attacker defender) in
            if (is_special (fst a)) then 0.
            else
              if (1. /. (snd a)) > ((1. /. ((snd a) *. increase)) +. 1.) then chance
              else 0.
            in
          let spe_up_weight ((increase, chance): float * float) : float = 
            if ((speed_ratio <= 1.) && (speed_ratio >= (1. /. increase))) then chance else 0.
            in
          let def_up_weight ((increase, chance): float * float) : float =
            let a = (strongest_attack defender attacker) in
            if (is_special (fst a)) then 0.
            else
              if (1. /. (snd a)) < ((increase /. (snd a)) +. 1.) then chance
              else 0.
            in
          let acc_up_weight ((increase, chance): float * float) : float =
            0.
            in
          let att_down_weight ((decrease, chance): float * float) : float =
            let a = (strongest_attack defender attacker) in
            if (is_special (fst a)) then 0.
            else
              if (1. /. (snd a)) < (1. /. ((snd a) *. decrease)) +. 1. then chance *. 0.5
              else 0.
            in
          let spe_down_weight ((decrease, chance): float * float) : float =
            if ((speed_ratio <= 1.) && (speed_ratio >= decrease)) then chance *. 0.5
              else 0.
            in
          let def_down_weight ((decrease, chance): float * float) : float =
            let a = (strongest_attack attacker defender) in
            if (is_special (fst a)) then 0.
            else
              if (1. /. (snd a)) > (decrease /. (snd a)) +. 1. then chance *. 0.5
              else 0.
            in
          let acc_down_weight ((decrease, chance): float * float) : float =
            let a = (strongest_attack defender attacker) in
            if (is_special (fst a)) then 0.
            else
              if (1. /. (snd a)) > (1. /. ((snd a) *. decrease)) +. 1. then chance *. 0.5
              else 0.
            in          
          (attack_power attacker defender att) +. poison_weight (poison_effect att) +. confuse_weight (confused_effect att) +.
          paralysis_weight (paralyze_effect att) +. sleep_weight (sleep_effect att) +. freeze_weight (freeze_effect att) +.
          sleep_weight (sleep_effect att) +. att_up_weight (attack_up attacker att) +. spe_up_weight (speed_up attacker att) +. 
          def_up_weight (defense_up attacker att) +. acc_up_weight (accuracy_up attacker att) +. 
          att_down_weight (attack_down defender att) +. spe_up_weight (speed_down defender att) +. 
          def_down_weight (defense_down defender att) +. acc_down_weight (accuracy_down defender att)
          in
        
        (* calculates the optimal attack *)  
        let optimal_attack_helper (attacking_team: team_data) (defending_team: team_data) : attack =  
          let attacking_pkmn = List.hd (fst attacking_team) in
          let defending_pkmn = List.hd (fst defending_team) in
          let primary = 
            let optimal = ref attacking_pkmn.first_attack in
            if (!optimal.pp_remaining < attacking_pkmn.second_attack.pp_remaining) && 
              (attack_power attacking_pkmn defending_pkmn attacking_pkmn.second_attack >= 1.)
              then optimal := attacking_pkmn.second_attack;
            if (!optimal.pp_remaining < attacking_pkmn.third_attack.pp_remaining) && 
              (attack_power attacking_pkmn defending_pkmn attacking_pkmn.third_attack >= 1.)
              then optimal := attacking_pkmn.third_attack;
            if (!optimal.pp_remaining < attacking_pkmn.fourth_attack.pp_remaining) && 
              (attack_power attacking_pkmn defending_pkmn attacking_pkmn.fourth_attack >= 1.)
              then optimal := attacking_pkmn.fourth_attack;
            !optimal
            in
          let secondary = 
            let optimal = ref attacking_pkmn.fourth_attack in
            if ((calculate_weights attacking_pkmn defending_pkmn !optimal) < 
              (calculate_weights attacking_pkmn defending_pkmn attacking_pkmn.first_attack)) &&
              (attacking_pkmn.first_attack.pp_remaining > 0)
              then optimal := attacking_pkmn.first_attack;
            if ((calculate_weights attacking_pkmn defending_pkmn !optimal) < 
              (calculate_weights attacking_pkmn defending_pkmn attacking_pkmn.second_attack)) &&
              (attacking_pkmn.second_attack.pp_remaining > 0)
              then optimal := attacking_pkmn.second_attack;
            if ((calculate_weights attacking_pkmn defending_pkmn !optimal) < 
              (calculate_weights attacking_pkmn defending_pkmn attacking_pkmn.third_attack)) &&
              (attacking_pkmn.third_attack.pp_remaining > 0)
              then optimal := attacking_pkmn.third_attack;
            if ((calculate_weights attacking_pkmn defending_pkmn !optimal) < 
              (calculate_weights attacking_pkmn defending_pkmn attacking_pkmn.fourth_attack)) &&
              (attacking_pkmn.fourth_attack.pp_remaining > 0)
              then optimal := attacking_pkmn.fourth_attack;            
            !optimal
            in 
             if (attack_power attacking_pkmn defending_pkmn primary >= 1.) then primary else secondary                                   
          in 
                                   
        let switch (attacking_team: team_data) (defending_team: team_data) : steammon option = 
          let attacking_pkmn = List.hd (fst attacking_team) in
          let defending_pkmn = List.hd (fst defending_team) in
          let team = List.filter (fun s -> s.curr_hp > 0) (List.tl (fst attacking_team)) in
          let weaknesses (s : steammon) : float =
            let helper (t : steamtype) : float = 
              match defending_pkmn.first_type with
              | Some t1 -> begin
                match defending_pkmn.second_type with
                | Some t2 -> weakness t t1 *. weakness t t2
                | None -> weakness t t1
                end
              | None -> 1.  
            in
            match s.first_type with
            | Some t1 -> begin
              match defending_pkmn.second_type with
              | Some t2 -> helper t2 *. helper t1
              | None -> helper t1
              end
            | None -> 1.
            in
          let best_steammon = 
            if List.length team = 1 then
              List.hd team
            else  
              List.fold_left (fun a x -> if (weaknesses a < weaknesses x) then x else a) (List.hd team) (List.tl team)  
            in
          let starter_hurt = 
            let a = attacking_pkmn.mods.attack_mod + attacking_pkmn.mods.speed_mod + attacking_pkmn.mods.defense_mod in
            let b = attacking_pkmn.mods.accuracy_mod in
            let c = if (List.mem Confused attacking_pkmn.status) then 1 else 0 in
            (2 * a + 3 * b + 5 * c) > 6 
            in
          if (attacking_pkmn.curr_hp = 0) then
            Some best_steammon
          else if (starter_hurt) then
            Some best_steammon
          else None            
          in
        
        let items (attacking_team: team_data) (defending_team: team_data) : (item * steammon) option=
          let attacking_pkmn = List.hd (fst attacking_team) in
          let defending_pkmn = List.hd (fst defending_team) in
          let [ethers; max_potions; revives; full_heals; xAttacks; xSpeeds; xDefenses; xAccuracies] = snd attacking_team in
          if (((snd (strongest_attack defending_pkmn attacking_pkmn) >= 1.0) && 
            ((float_of_int attacking_pkmn.curr_hp /. float_of_int attacking_pkmn.max_hp) <= (1. /. 3.))) || 
            ((float_of_int attacking_pkmn.curr_hp /. float_of_int attacking_pkmn.max_hp) <= (1. /. 10.))) &&
            (max_potions > 0)
            then Some (MaxPotion , attacking_pkmn)
          else if ((List.length (List.filter (fun s -> s.curr_hp = 0) (fst attacking_team))) > 0) && (revives > 0)
            then Some (Revive , (List.hd (List.filter (fun s -> s.curr_hp = 0) (fst attacking_team))))
          else if ((List.mem Paralyzed attacking_pkmn.status) || (List.mem Poisoned attacking_pkmn.status)) && (full_heals > 0)
            then Some (FullHeal , attacking_pkmn)
          else None
          in
        
        let helper (a: team_data) (b: team_data) : action =
          match switch a b with
          | Some s -> SwitchSteammon s.species
          | None ->
            match items a b with
            | Some (i, s) -> UseItem (i,s.species)
            | None -> UseAttack ((optimal_attack_helper a b).name)
          in
        begin
        match c with 
        | Red -> helper red_data blue_data
        | _ -> helper blue_data red_data
        end
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
        
    | PickInventoryRequest (gs) -> 
      let cash = ref cINITIAL_CASH in
      let maxPotionCash = !cash / 2 in
      let reviveCash = !cash / 3 in
      let(r_data, b_data) = gs in
      let (steammon, inventory) = 
        match c with 
        | Red -> r_data
        | Blue -> b_data in
      let ethers = 0 in
      let max_potions = (!cash / 2) / cCOST_MAXPOTION in
      cash := !cash - (max_potions * cCOST_MAXPOTION);
      let revives = (!cash / 3) / cCOST_REVIVE in
      cash := !cash - (revives * cCOST_REVIVE);
      let full_heals = (!cash / 6) / cCOST_FULLHEAL in 
      cash := !cash - (full_heals * cCOST_FULLHEAL);
      let xattacks = 0 in
      let xdefenses = 0 in
      let xaccuracies = 0 in
      let xspeeds = 0 in
      PickInventory(
      [ethers; max_potions; revives; full_heals;
      xattacks; xdefenses; xaccuracies; xspeeds])
      
    let () = run_bot handle_request
