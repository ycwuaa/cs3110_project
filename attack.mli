(** attack.mli ****************************************************************)
(** Handles checking if a desired attack is valid and performing the  attack **)
(** mechanic ******************************************************************)

open GameState


(** returns a pseudo-random int between 1 and 6 (inclusive)*)
val roll_dice : unit -> int

(** takes in two sorted lists representing the outcomes of attckers and defender
  * die rolls; returns a pair of ints reprsenting the number or successes for
  * attacker and defender respectively*)
val attack_outcome : int list -> int list -> int*int

(** returns and updated gamestate with n pieces removed from the given
  * territory *)
val remove_pieces : t -> int -> territory -> t

(** checks if a territory is captured after an attack; returns true if all
 ** pieces have been removed from the territory, false otherwise *)
val is_captured : t -> territory -> bool

(** updates the game state by moving n pieces of allied with the owner of the
  * first territory to the second territory, assumes player has at least [n]
  * pieces in [terr1]*)
val invade : t -> int -> territory -> territory -> t

(** returns true if there are enough peices in territory for its owner to
  * perform an attack with n pieces

    TODO: move to checking*)
val check_min_pieces : t  -> int -> territory -> bool

(** returns the maximum number of tie the player occupying [terr] can roll as
  * a defender *)
val max_defend : t -> territory -> int