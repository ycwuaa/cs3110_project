(** AI.mli *)

open GameState

(** choose a name for each AI at the beginning of the game. *)
val choose_name : unit -> string

(** at the beginning of the game, receive a few additional armies; return
 * a list of where to put these extra armies *)
val place_original_armies: t -> int -> (int * territory) list

(** takes in a GameState and the number of extra armies received, and returns
 * a territory and int containing the newly added armies that the AI had
 * added *)
val place_new_armies : t -> int -> (int * territory)

(** takes in a GameState and returns Some (from, to, dice) if the AI wants to
 * attack the [to] territory with the army on the [from] territory using [dice]
 * number of dice, or None if no attack is desired *)
val choose_attack : t -> (territory * territory * int) option

(** takes in a GameState, a territory to move from, a territory to move to, and
 * the minimum number of armies to move; returns the chosen number of armies to
 * move *)
val choose_move_conquerors : t -> territory -> territory -> int -> int

(** takes in a GameState and returns Some (from, to, num) if the AI wants to
 * move [num] armies from the [from] territory to the [to] territory, or None if
 * no redistribution is desired *)
val redistribute_armies : t -> (territory * territory * int) option

(** takes in a GameState, a territory attacking from, a territory being
 * attacked, and the number of dice enemy is using and maximum number of dice
 * the player can defend with and returns the chosen number of dice *)
val choose_dice : t -> territory -> territory -> int -> int -> int
