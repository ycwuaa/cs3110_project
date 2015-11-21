(** beginTurn.mli : ***********************************************************)
(** handles distributing army pieces and placing them on the map **************)

open GameState

(** returns number of pieces to award to player with player_id who has control
  * of territories in the game state; checks for continent control and awards
  * accordingly*)
val award_pieces : t -> player_id -> int

(** returns a game state with n pieces aligned with player_id placed on given
  * territory*)
val place_piece : t -> int -> player_id -> territory -> t