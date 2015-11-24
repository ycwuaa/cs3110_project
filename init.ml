open GameState

let distribute_territory (state:t) =

  (** check with the default value later*)
  let terro_list = get_territories state (-1) in
  let player_list = get_player_id_list state in
  let num_player = List.length player_list in
  let counter = ref 0 in
  let start = ref 0 in
  let check () =
    if ((!counter) mod num_player) = 0 then
      counter:=0; start:=Random.int num_player
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
  distribute_helper state terro_list

let set_first_player (state:t) =
  let player_list = get_player_id_list state in
  let num_player = List.length player_list in
  let cur = Random.int num_player in
  set_active_player state (List.nth player_list cur)