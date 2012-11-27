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
          let statsum = x.attack + x.spl_attack + x.defense +
            x.spl_defense + x.speed in
          Hashtbl.add (x.species) statsum
        ) () sp;
        if Hashtbl.length > 0 then
          let (name, statsum) = 
            Hashtbl.fold (fun k (n, s) (species, stats) -> 
              if s > stats then (n, s) else acc) steammon_tbl ("", 0) in name
        else failwith "no steammon available to pick!"
    | ActionRequest (gr) ->
        let (r_data, b_data) = gr in
        let team = if c = Red then r_data else b_data in
        let (steammon, [a;b;c;d;e;f;g;h]) = team in
        (match mons with
        | h::t ->
	    if h.curr_hp < h.max_hp && b > 0 then UseItem(MaxPotion,h.species) else
            if (h.first_attack).pp_remaining >0 then
              let _ = print_endline (h.species ^ "used " ^ ((h.first_attack).name)) in
                UseAttack((h.first_attack).name)
            else if ((h.second_attack).pp_remaining > 0) then
              let _ = print_endline (h.species ^ "used " ^ ((h.second_attack).name)) in
                UseAttack((h.second_attack).name)
            else if ((h.third_attack).pp_remaining >0) then
              let _ = print_endline (h.species ^ "used " ^ ((h.third_attack).name)) in
                UseAttack((h.third_attack).name)
            else
              let _ = print_endline (h.species ^ "used " ^ ((h.fourth_attack).name)) in
                UseAttack((h.fourth_attack).name)
        | _ -> failwith "WHAT IN THE NAME OF ZARDOZ HAPPENED HERE")
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
