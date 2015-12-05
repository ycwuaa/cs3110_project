(** beginTurn.ml: *************************************************************)
(** handles distributing army pieces and placing them on the map **************)

open GameState

(** returns number of pieces to award to player with player_id who has control
  * of territories in the game state; checks for continent control and awards
  * accordingly*)
let award_pieces  (state: t)  (pid: player_id): int =
  (*get territories pid controls*)
  let territories = get_territories state pid in

  (*award troops based on # territories held*)
  let num_held = List.length territories in
  let to_award = num_held / 3 in
  let continent_l = get_continents state pid in
  let rec continent_award acc = function
    | [] -> acc
    | h::t -> let new_acc = get_continent_pt state h in
              continent_award (new_acc+acc) t
  in
  let cont_aw = continent_award 0 continent_l in
  if to_award < 3 then (3+cont_aw) else (to_award+cont_aw)

(** returns a new state, checks if [pid] owns any continents in [state] *)
let continent_ownership (state: t) (pid: player_id): t =
  (** returns true if all elements of [cont_list] are in [terr_list] *)
  let rec check_cont_ownership terr_list cont_list =
    match cont_list with
    | [] -> true
    | hd :: tl -> (List.mem hd terr_list) && (check_cont_ownership terr_list tl)
  in

  (** sets pid as owner of a continents that he owns, returns an updated
    * gamestate *)
  let rec check_continents gs t_list c_list =
    match c_list with
    | [] -> gs
    | hd :: tl ->
      let c_t_list = get_continent_territories gs hd in
      if check_cont_ownership t_list c_t_list
        then let new_gs = set_continent_owner gs pid hd in
        check_continents new_gs t_list tl
      else check_continents gs t_list tl
  in
  let territories = get_territories state pid in
  let continents = get_all_continents state in

  check_continents state territories continents


(** returns a game state with n pieces aligned with player_id placed on given
  * territory, assumes pid controls terr*)
let place_piece (state: t) (n: int) (pid: player_id) (terr: territory): t =
  (* (*if pid is not yet owner, refuse to place pieces?*)
  let territories = get_territories state pid in
  if not (List.mem terr territories)
    then failwith "cannot place unit on territory you do not control"
  else *)
    (*add existing # of armies on terr to new additions*)
    let existing = get_armies state terr in
    let new_num = existing + n in

    (*update num on terr and return new state*)
    set_num_armies state terr new_num
