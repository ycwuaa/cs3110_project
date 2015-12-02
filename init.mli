(** init.mli *)

open GameState

(** distribute the terriyories randomly to all the players and give on army
  * on each territory*)
val distribute_territory : t -> t

(** place the army as the player indicates : who(player_id), how many and where*)
val place_army : t -> (int*territory) list -> t

(** decide which player to start from by taking in the player id*)
val set_first_player : t -> t
