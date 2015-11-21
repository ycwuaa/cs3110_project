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

  (*TODO: check if any continents are held and award points accordingly*)
  to_award


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
