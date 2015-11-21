(** endTurn.mli *)

open GameState

(** move the army from territory to territory
  * NOTICE: territory on the way all need to be the current player's
  * take in the playerid, from territory to territory*)
val move_army : t -> player_id -> int -> territory -> territory -> t

(** check whether any player has won the game or not at the end of this turn
  * if no one win yet, return None else give back the int option contiaining
  * the winning player_id*)
val check_win : t -> player_id option

(** check whether any player is out at the end of this turn
  * if no one is out, give back empty list else give back all the playerid of who
  * already lost in a int list option*)
val check_defeated : t -> player_id list

