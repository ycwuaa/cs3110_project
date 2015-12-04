open GameState

let distribute_territory (gs:t) =
  (* http://stackoverflow.com/questions/15095541/
 * how-to-shuffle-list-in-on-in-ocaml *)
  let shuffle d =
    let () = Random.self_init () in
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond
  in

  let terrs = shuffle (get_all_territories gs) in
  let cur = get_active_player gs in
  let give ngs terr =
    let p = get_active_player ngs in
    set_territory_owner ngs terr p |>
    fun x -> set_num_armies x terr 1 |>
    set_next_player
  in
  (List.fold_left give gs terrs) |>
  fun x -> set_active_player x cur

let place_army (state:t) assigned_l =
  let rec helper cur_state = function
    | [] -> cur_state
    | (num, terri)::t -> let ori = get_armies cur_state terri in
                         let new_state =
                           (set_num_armies cur_state terri (num+ori)) in
                         helper new_state t
  in
  helper state assigned_l

let set_first_player (state:t) =
  let player_list = get_player_id_list state in
  let num_player = List.length player_list in
  let cur = Random.int num_player in
  set_active_player state (List.nth player_list cur)
