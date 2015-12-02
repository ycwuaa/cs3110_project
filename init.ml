open GameState

let distribute_territory (state:t) =

  (** check with the default value later*)
  let terro_list = get_territories state no_one in
  let player_list = get_player_id_list state in
  let num_player = List.length player_list in
  let counter = ref 0 in
  let start = ref 0 in
  let check () =
    if ((!counter) mod num_player) = 0 then
      (counter:=0; start:=(Random.int num_player))
    else ()
  in
  let rec distribute_helper cur_state remain_terro =
    check ();
    match terro_list with
    | [] -> cur_state
    | h::t ->
      let cur_player = List.nth player_list ((!start) mod num_player) in
      let new_state = set_territory_owner cur_state h cur_player in
      counter:=(!counter)+1; start:=(!start)+1;
      distribute_helper new_state t
  in
  let after_set_owner = distribute_helper state terro_list in
  let rec give_one_army cur_state = function
    | [] -> cur_state
    | h::t -> let new_state = set_num_armies cur_state h 1 in
              give_one_army new_state t
  in
  give_one_army after_set_owner (get_all_territories after_set_owner)

let place_army (state:t) assigned_l =
  let rec helper cur_state = function
    | [] -> cur_state
    | (num, terri)::t -> let new_state = (set_num_armies cur_state terri num) in
                         new_state
  in
  helper state assigned_l

let set_first_player (state:t) =
  let player_list = get_player_id_list state in
  let num_player = List.length player_list in
  let cur = Random.int num_player in
  set_active_player state (List.nth player_list cur)
