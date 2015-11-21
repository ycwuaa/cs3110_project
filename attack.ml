(** attack.mli ****************************************************************)
(** Handles checking if a desired attack is valid and performing the  attack **)
(** mechanic ******************************************************************)

open GameState


(** returns a pseudo-random int between 1 and 6 (inclusive)*)
let roll_dice () : int =

(** takes in two sorted lists representing the outcomes of attckers and defender
  * die rolls; returns a pair of ints reprsenting the number or successes for
  * attacker and defender respectively*)
let attack_outcome (attacker_rolls: int list) (defender_rolls: int list) :
  int*int =
  failwith "not implemented"

(** returns and updated gamestate with n pieces removed from the given
  * territory *)
let remove_pieces (state: t) (n: int) (terr: territory): t =
  failwith "not implemented"

(** checks if a territory is captured after an attack; returns true if all
 ** pieces have been removed from the territory, false otherwise *)
let val is_captured (state: t) (terr: territory): bool =
  failwith "not implemented"

(** updates the game state by moving n pieces of allied with the owner of the
  * first territory to the second territory *)
let val invade (state: t) (n: int) (terr1: territory) (terr2: territory): t =
  failwith "not implemented"

(** returns true if the first territory is adjacent to the second in gamestate*)
let check_adjacent (state: t) (terr1: territory) (terr2: territory): bool =
  failwith "not implemented"

(** returns true if there are enough peices in territory for its owner to
  * perform an attack with n pieces*)
let check_min_pieces (state: t)  (n: int) (terr: territory): bool =
  failwith "not implemented"

