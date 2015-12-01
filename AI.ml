(** AI.mli *)

open GameState


(* helper debug function to print out a territory list *)
let rec print_list tostring = function
| [] -> ()
| h::t ->
  let _ = Printf.printf " @ %s\n" (tostring h) in
  print_list tostring t



let choose_name =
  let curr_int = ref 1 in
  fun () ->
    let returnVal = "CPU " ^ (string_of_int !curr_int) in
    let _ = incr curr_int in
    returnVal

(* current strategy: place equally everywhere *)
let place_original_armies (gs:t) (num_new:int) =
  let p' = get_active_player gs in
  let my_terr = get_territories gs p' in
  let num_terr = List.length my_terr in

  (* divide up the [dividend] into a list of length [divisor] *)
  let rec divide_up dividend divisor acc =
    if dividend <= 0 then
      acc
    else if dividend < divisor then
      dividend::acc
    else
      let quotient = dividend / divisor in
      divide_up (dividend - quotient) (divisor - 1) (quotient::acc)
    in

  let number_list = divide_up num_new num_terr [] in
  List.combine number_list my_terr

(* current strategy: place everything on the first territory found *)
let place_new_armies (gs:t) (num_new:int) =
  let p' = get_active_player gs in
  let my_terr = get_territories gs p' in
  (num_new, List.hd my_terr)

(* current strategy: keep attacking from the first territory found with
 * armies > 1 to the first enemy territory found that neighbors it, or
 * do nothing if the first or second conditions are not met *)
let choose_attack (gs:t) =
  let p' = get_active_player gs in
  let my_terr = get_territories gs p' in
  let usable_terr = List.filter (fun terr -> get_armies gs terr > 1) my_terr in

  match usable_terr with
  | [] -> None (* no territories with at least one army on it *)
  | from::_ ->
      let neighbors = get_adjacency gs from in
      let enemy_neighbors = List.filter (fun terr -> (get_territory_owner gs terr) <> p') neighbors in
      match enemy_neighbors with
      | [] -> None (* our first territory is landlocked. For now, just do nothing *)
      | toward::_ ->
        (* roll 3 dice if possible; otherwise, roll the maximum allowed
         * (number of armies on "from," minus 1) *)
        let num_from = min 3 (get_armies gs from - 1) in
        Some (from, toward, num_from)

(* current strategy: move everyone possible *)
let choose_move_conquerors (gs:t) (from:territory) (toward:territory)
  (minMove:int) =
  min (minMove) (get_armies gs from - 1)

(* current strategy: do nothing *)
let redistribute_armies  (gs:t) =
  None

(* current strategy: always defend with the most dice possible *)
let choose_dice (gs:t) (from:territory) (toward:territory) (num_enemy:int)
  (max_dice:int) =
  max_dice