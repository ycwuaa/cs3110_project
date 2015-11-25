open GameState
open Assertions

let state = new_state () ;

(*tests on initial state*)
TEST_UNIT "get_player_id_list" = get_player_id_list state === []

TEST_UNIT "get_territories (non-empty)" =
  assert(List.length (get_territories state no_one) > 0)

TEST_UNIT "get_is_human" = 0 === 0 (*need better error handling*)

TEST_UNIT "get_name" = 0 === 0 (*need better error handling*)

TEST_UNIT "get_active_player" = get_active_player state === no_one

(*returns true if every territory held by no_one has 0 pieces, false otherwise*)
let rec get_armies_init_helper territories =
  match territories with
  | [ ] -> true
  | hd :: tl -> let num_armies = get_armies state hd in
    if num_armies = 0 then (true  && (get_armies_init_helper tl))
    else false

TEST_UNIT "get_armies" =
  assert(get_armies_init_helper (get_territories state no_one))

(*returns true if no_one is the owner of all territories in list*)
let rec get_t_owners_helper territories =
  match territories with
  | [] -> true
  | hd :: tl -> let owner = get_territory_owner state hd in
    if owner = no_one then (true && (get_t_owners_helper tl))
    else false
TEST_UNIT "get_territory_owner" =
  assert(get_t_owners_helper (get_territories state no_one))

TEST_UNIT "get_continents" = get_continents state no_one === []

let terr1 = List.hd (get_territories state no_one)

let state = set_num_armies state terr1 2

TEST_UNIT "set_num_armies" = get_armies state terr1 === 2

let p1 = create_player 1

let old_player_list = get_player_id_list state

let state = add_player state p1 "test player1" true

TEST_UNIT "add_player" =
  assert(List.length (get_player_id_list state) > List.length old_player_list)

TEST_UNIT "get_is_human" = get_is_human state p1 === true

TEST_UNIT "get_name" = get_name state p1 === "test player1"

let state = set_territory_owner state terr1 p1

TEST_UNIT "set_territory_owner" = get_territory_owner state terr1 === p1

TEST_UNIT "get_active_player" = get_active_player state === no_one

TEST_UNIT "debug" = List.length (get_player_id_list state) === 1

let state = set_next_player state

TEST_UNIT "set_next_player" = get_active_player state === p1

let state = set_active_player state no_one

TEST_UNIT "set_active_player" = get_active_player state === no_one

let state = remove_player state p1

TEST_UNIT "remove_player" = get_player_id_list state === old_player_list

(* returns the first continent in c_list or None if there are no continents *)
let get_first_continent c_list =
  match c_list with
  | [] -> None
  | hd :: tl -> Some hd

let cont = get_first_continent (get_all_continents state)

(** returns true if [id] is correctly set as owner of [c], or if [cont] is
  * None, returns true if [id] owns no continents
  * precondition: id owns no continents *)
let check_cont_ownership id cont =
    match cont with
    | None -> if (get_continents state id) = [] then true else false
    | Some c -> (
      let state1 = set_continent_owner state id c in
      if (get_continents state1 id) = [c] then true else false)


TEST_UNIT "get_continents" = get_continents state p1 === []

TEST_UNIT "get_continents" = get_continents state no_one === []

TEST_UNIT "get_continents" = assert(check_cont_ownership p1 cont)

