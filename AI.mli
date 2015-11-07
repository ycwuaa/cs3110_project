(** AI.mli *)

open GameState

(** takes in a GameState and the number of extra armies received, and returns
 * the new GameState with the armies placed in locations as chosen by the AI *)
val place_new_armies : t -> int -> t

(** takes in a GameState and returns Some (from, to, dice) if the AI wants to
 * attack the [to] territory with the army on the [from] territory using [dice]
 * number of dice, or None if no attack is desired *)
val choose_attack : t -> (territory * territory * int) option

(** takes in a GameState, a territory to move from, a territory to move to, and
 * the minimum number of armies to move; returns the chosen number of armies to
 * move *)
val choose_move_conquerors : t -> territory -> territory -> int -> int

(** takes in a GameState and returns Some (from, to, num) if the AI wants to
 * [num] armies from the [from] territory to the [to] territory, or None if
 * no redistribution is desired *)
val redistribute_armies : t -> int -> (territory * territory * int) option