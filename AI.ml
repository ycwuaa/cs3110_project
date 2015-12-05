(** AI.mli *)

open GameState

(* for internal judg(e)ments about which move to make
 * higher rank = more preferable *)
type rank = float
let army_size_multiplier = 0.9 (*prioritize difference in army size*)
let near_ownership_multiplier = 2.0 (*prioritize my almost-owned continents*)
let non_boundary_penalty = -5.0 (*avoid non-boundaries of owned region*)
let internal_conquest_multiplier = 2.5 (*prioritize boundaries of owned region*)
let enemy_ownership_multiplier = 2.0 (*prioritize disrupting enemy continents*)
let my_ownership_multiplier = 2.6 (*prioritize defending own continents*)



(** HELPER FUNCTIONS **)

let rank_of_bool b = if b then 1.0 else 0.0

(* helper debug function to print out a territory list *)
let print_list tostring lst =
  let rec helper num = function
  | [] -> ()
  | h::t ->
    let _ = Printf.printf " %d. %s\n" num (tostring h) in
    helper (num + 1) t
  in
  helper 1 lst

(* cmp function for sorting a pair, as specified by List.sort and Array.sort
 * reverse the call to compare so that the sort is in decreasing order *)
let cmp_ranks_2 (rnk1, _) (rnk2, _) = compare rnk2 rnk1
(* cmp function for sorting a triple, as specified by List.sort and Array.sort
 * reverse the call to compare so that the sort is in decreasing order *)
let cmp_ranks_3 (rnk1, _, _) (rnk2, _, _) = compare rnk2 rnk1

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

(* returns true if this territory is in a completely owned continent;
 * false otherwise *)
let is_owned_continent (gs:t) (terr:territory) : bool =
  let cont = get_continent_of_terr gs terr in
  (* check if the no-one condition on get_continents is true
   * TODO: ask if this'll work with the implementation
   * check if NOT no one owns the whole continent *)
  not ( List.mem cont (get_continents gs no_one) )

(* returns true if this territory cannot attack an enemy territory;
 * false otherwise
 * precondition: [terr] is owned by the active player *)
let is_nonboundary (gs:t) (terr:territory) : bool =
  let p' = get_active_player gs in
  let neighbors = get_adjacency gs terr in
  let enemy_neighbors =
    List.filter (fun terr -> (get_territory_owner gs terr) <> p') neighbors in
  enemy_neighbors = []

(* returns true if this territory can attack an enemy territory in a continent I
 * own most of (more than 50%);
 * false otherwise
 * precondition: [terr] is the owned by the active player *)
let internal_conquest (gs:t) (terr:territory) : bool =
  let p' = get_active_player gs in
  let neighbors = get_adjacency gs terr in

  let is_my_continent = List.map
    (fun (cont,rnk) -> (cont, rnk > 0.5))
    (get_my_continents gs) in

  let internal_enemy_neighbors =
    List.filter
      (fun terr ->
        (*enemy*)
        (get_territory_owner gs terr) <> p'
        (*internal*)
        && List.assoc (get_continent_of_terr gs terr) is_my_continent )
      neighbors in
  internal_enemy_neighbors <> []


(* rate the attacking option from [from] to [target]; return a
 * (rank, from, target) tuple *)
let rank_attack_option gs (from, target) : rank * territory * territory =
  (* target territories that are easier to take over *)
  let army_diff = float_of_int (get_armies gs from - get_armies gs target) in
  (* prioritize taking over continents that I already own most of *)
  let near_ownership_bonus =
    List.assoc (get_continent_of_terr gs target) (get_my_continents gs) in
  (* prioritize disrupting enemy continents *)
  let enemy_continent_bonus = rank_of_bool (is_owned_continent gs target) in

  (
      army_diff       *. army_size_multiplier
   +. near_ownership_bonus *. near_ownership_multiplier
   +. enemy_continent_bonus *. enemy_ownership_multiplier
   , from, target
  )

(* rate the value of a given territory [terr] ; return a (rank, terr) tuple *)
let rank_place_option gs terr : rank * territory =
  let near_ownership_bonus =
    List.assoc (get_continent_of_terr gs terr) (get_my_continents gs) in
  let ownership_bonus = rank_of_bool (is_owned_continent gs terr) in
  let nonboundary_value = rank_of_bool (is_nonboundary gs terr) in
  let internal_conquest_bonus = rank_of_bool (internal_conquest gs terr) in

  (
      near_ownership_bonus *. near_ownership_multiplier
   +. ownership_bonus *. my_ownership_multiplier
   +. nonboundary_value *. non_boundary_penalty
   +. internal_conquest_bonus *. internal_conquest_multiplier
   , terr
  )



(** EXPOSED FUNCTIONS **)

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

  (* divide up the int [dividend] into a list of length [divisor] whose sum is
   * [dividend] and whose values are roughly equal
   * precondition: there are at least as many dividend >= divisor *)
  let rec divide_up dividend divisor acc =
    if divisor <= 0 then
      acc
    else
      let quotient =
        if dividend > 0 then
          max 1 (dividend / divisor) (* if any left, round up *)
        else
          0 (* none left: can't add any more *)
        in
      divide_up (dividend - quotient) (divisor - 1) (quotient::acc)
    in

  let number_list = divide_up num_new num_terr [] in

  (* let _ = Printf.printf "%s" (get_name gs p') in *)
  let () = print_list (string_of_territory gs) my_terr in
  let () = Printf.printf "\nNumber of territories: %d\n" num_terr in
  let () = Printf.printf "\nNumber of armies: %d\n" num_new in
  let () = print_list (string_of_int) number_list in

  List.combine number_list my_terr

(* current strategy: place everything on the first territory found *)
let place_new_armies (gs:t) (num_new:int) =
  let p' = get_active_player gs in
  let my_terr = get_territories gs p' in

  let all_ranks = List.map (rank_place_option gs) my_terr in
  let sorted_ranks = List.sort cmp_ranks_2 all_ranks in
  let _ = print_list (fun (rnk, t1) -> Printf.sprintf "%7f: %12s" rnk t1) sorted_ranks in

  match List.hd sorted_ranks with
  | (rnk, terr) ->
    (num_new, terr) (* TODO: put in more places *)


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
  let sorted_ranks = List.sort cmp_ranks_3 all_ranks in
  let _ = print_list (fun (rnk, t1, t2) -> Printf.sprintf "%7f: %12s to %12s" rnk t1 t2) sorted_ranks in

  if sorted_ranks = [] then
    None
  else
    match List.hd sorted_ranks with
    | (rnk, _, _) when rnk < 1.0 -> None
    | (rnk, from, target) ->
      let num_armies = (get_armies gs from) - 1 in
      Some (from, target, min num_armies 3)

(* current strategy: move everyone possible *)
let choose_move_conquerors (gs:t) (from:territory) (target:territory)
  (minMove:int) =
  get_armies gs from - 1

(* current strategy: find any armies not on the boundary and place them elsewhere *)
let redistribute_armies  (gs:t) =
  let p' = get_active_player gs in
  let my_terr = get_territories gs p' in
  let nonboundary =
    List.filter
    (fun terr -> is_nonboundary gs terr && get_armies gs terr > 1)
    my_terr
    in
  if nonboundary = [] then
    None
  else
    let from = List.hd nonboundary in
    let number = get_armies gs from - 1 in
    let (_, target) = place_new_armies gs number in
    Some (from, target, number)

(* current strategy: always defend with the most dice possible *)
let choose_dice (gs:t) (from:territory) (target:territory) (num_enemy:int)
  (max_dice:int) =
  max_dice