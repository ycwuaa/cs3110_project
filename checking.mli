(** checking.ml *)
(* functions for checking valid input *)

open GameState

(** returns true if the first territory is adjacent to the second in gamestate*)
val check_adjacent : t -> territory -> territory -> bool

(** returns true if there are enough pieces in territory for its owner to
  * perform an attack with n pieces (need to have at least n+1 pieces in the
  * territory but n cannot be greater than 3) *)
val check_min_pieces : t -> int-> territory -> bool

(** returns true if the territory given is owned by the given player
  *)
val check_owner : t -> territory -> player_id