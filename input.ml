(** input.mli *)
(** Handles all game actions by human players*)

open GameState

(** Gets the start configuration for the game (# of human, # of AI) *)
let choose_start () =
  failwith "TODO"

(** Returns whether the player wants to play again. *)
let play_again () =
  failwith "TODO"

(** Gets the player name at the beginning of the game.*)
let choose_name () =
  failwith "TODO"

(** at the beginning of the game, receive a few additional armies; return
 * a list of where to put these extra armies *)
let place_original_armies gs num =
  failwith "TODO"

(** takes in a GameState and the number of extra armies received, and returns
 * a territory and int containing the newly added armies that the human had
 * added *)
let place_new_armies gs num =
  failwith "TODO"

(** takes in a GameState and returns Some (from, to, dice) if player will
 * attack the [to] territory with the army on the [from] territory using [dice]
 * number of dice, or None if no attack is desired *)
let choose_attack gs =
  failwith "TODO"

(** takes in a GameState, a territory to move from, a territory to move to, and
 * the minimum number of armies to move; returns the chosen number of armies to
 * move *)
let choose_move_conquerors gs from to min =
  failwith "TODO"

(** takes in a GameState and returns Some (from, to, num) if player moves
 * [num] armies from the [from] territory to the [to] territory, or None if
 * no redistribution is desired *)
let redistribute_armies gs =
  failwith "TODO"