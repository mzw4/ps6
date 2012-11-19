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
        available <- Some (List.fold_left (fun a x -> if (x = st) then a else x::a) [] available)
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
  
  let attacker_helper (t_data: team_data) : team_data = 
    let (lst, inventory) = t_data in
    let starter = List.hd lst in
    if ((starter.first_attack ~= a) && (starter.second_attack ~= a) && 
      (starter.third_attack ~= a) && (starter.fourth_attack ~= a))
    then failwith "The starting steammon does not have this attack"
  in
  let defender_helper (t_data: team_data) : team_data = 
    let (
    let (lst, inventory) = t_data in
    let starter = List.hd lst in
  
  in
  match team with
  | Red -> set_game_data st ((attacker_helper red_data), (defender_helper blue_data))
  | Blue -> set_game_data st ((defender_helper red_data), (attacker_helper blue_data))

  
  