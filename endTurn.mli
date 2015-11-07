(** endTurn.mli *)

open GameState

(** move the army from territory to territory
  * NOTICE: territory on the way all need to be the current player's
  * take in the playerid, from territory to territory and return whether the
  * move is successful or not*)
val move_army int -> territory -> territory -> bool

(** check whether any player win the game or not at the end of this turn
  * if no one win yet, return None else give bakc the int option contiaining
  * the winning player_id*)
val check_win unit -> player_id option

(** check whether any player is out at the end of this turn
  * if no one is out, give back None else give back all the playerid of who
  * already lost in a int list option*)
val check_defeated unit -> player_id list option