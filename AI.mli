(** AI.mli *)

open GameState

(** takes in a GameState and the number of extra armies received, and returns
 * an association list representing the number of armies the AI wants to put on
 * each territory *)
val place_new_armies : t -> int -> (territory * int) list

(** takes in a GameState and returns Some (from, to) if the AI wants to attack
 * the [to] territory with the army on the [from] territory, or None if no
 * attack is desired *)
val choose_attack : t -> (territory * territory) option

(** takes in a GameState and returns Some (from, to, num) if the AI wants to
 * [num] armies from the [from] territory to the [to] territory, or None if
 * no redistribution is desired *)
val redistribute_armies : t -> int -> (territory * territory * int) option