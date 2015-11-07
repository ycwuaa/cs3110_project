(** init.mli *)

(** according to the number of player, distribute the infantry*)
val distribute_army unit -> unit

(** distribute the terriyories randomly to all the players*)
val distribute_territory unit -> unit

(** place the army as the player indicates : who(palyerid), how many and where*)
val place_army int -> int -> territory -> unit

(** decide which player to start from by taking in the player id*)
val set_first_player int -> unit

(** how to set first player? add new function in gameState?*)