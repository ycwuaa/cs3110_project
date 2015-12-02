(** AI.mli *)

open GameState

(* for internal judg(e)ments about which move to make
 * higher rank = more preferable *)
type rank = float

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

(* return a list of (continent, percentage of territories on that continent that
 * the active player owns) *)
let get_my_continents (gs:t) : (continent * float) list =
  let p' = get_active_player gs in

  (* return the number of territories in the continent owned by p' *)
  let num_my_territories cont =
    List.length (List.filter
      (fun terr -> (get_territory_owner gs terr) = p')
      (get_continent_territories gs cont) )
    in

  List.map (fun cont ->
    ( cont ,
      float_of_int ( num_my_territories cont ) /.
      float_of_int ( List.length (get_continent_territories gs cont) )
    )
  ) (get_all_continents gs)

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

(* return a list of all (friendly territory, target enemy territory) possible *)
let rec get_attack_options gs usable_terr acc =
  let p' = get_active_player gs in
  match usable_terr with
  | [] -> acc
  | h::t ->
    let neighbors = get_adjacency gs h in
    let enemy_neighbors =
      List.filter (fun terr -> (get_territory_owner gs terr) <> p') neighbors in
    get_attack_options gs t
      (List.map (fun terr -> (h, terr)) enemy_neighbors @ acc)

(* rate the attacking options from a given; return a (rank, from, target) tuple
 * currently just prioritize highest difference in army size *)
let rank_attack_option gs (from, target) : rank * territory * territory =
  (* target territories that are easier to take over *)
  let army_diff = float_of_int (get_armies gs from - get_armies gs target) in
  (* prioritize taking over continents that I already own most of *)
  let ownership_bonus = List.assoc (get_continent_of_terr gs target) (get_my_continents gs) in

  (
      army_diff       *. 0.9
   +. ownership_bonus *. 1.5

   , from, target
  )

(* cmp function for sorting, as specified by List.sort and Array.sort
 * reverse the call to compare so that the sort is in decreasing order *)
let cmp_ranks (rnk1, _, _) (rnk2, _, _) = compare rnk2 rnk1

(* current strategy: keep attacking from the first territory found with
 * armies > 1 to the first enemy territory found that neighbors it, or
 * do nothing if the first or second conditions are not met,
 * always attacking with the maximum possible *)
let choose_attack (gs:t) =
  let p' = get_active_player gs in
  let my_terr = get_territories gs p' in
  let usable_terr = List.filter (fun terr -> get_armies gs terr > 1) my_terr in

  let all_options = get_attack_options gs usable_terr [] in
  let all_ranks = List.map (rank_attack_option gs) all_options in
  let sorted_ranks = List.sort cmp_ranks all_ranks in
  let _ = print_list (fun (rnk, t1, t2) -> Printf.sprintf "%7f: %12s to %12s" rnk t1 t2) sorted_ranks in

  if sorted_ranks = [] then
    None
  else
    match List.hd sorted_ranks with
    | (rnk, from, target) when rnk < 1.0 -> None
    | (rnk, from, target) ->
      let num_armies = (get_armies gs from) - 1 in
      Some (from, target, min num_armies 3) (* TODO: put the constant 3 in another module as a contant? *)

(* current strategy: move everyone possible *)
let choose_move_conquerors (gs:t) (from:territory) (target:territory)
  (minMove:int) =
  min (minMove) (get_armies gs from - 1)

(* current strategy: do nothing *)
let redistribute_armies  (gs:t) =
  None

(* current strategy: always defend with the most dice possible *)
let choose_dice (gs:t) (from:territory) (target:territory) (num_enemy:int)
  (max_dice:int) =
  max_dice