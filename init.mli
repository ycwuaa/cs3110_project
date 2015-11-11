(** init.mli *)

open GameState

(** according to the number of player, distribute the infantry*)
val distribute_army : t -> unit -> unit

(** distribute the terriyories randomly to all the players*)
val distribute_territory : t -> unit -> unit

(** place the army as the player indicates : who(player_id), how many and where*)
val place_army : t -> player_id -> int -> territory -> unit

(** decide which player to start from by taking in the player id*)
val set_first_player : t -> player_id -> unit
