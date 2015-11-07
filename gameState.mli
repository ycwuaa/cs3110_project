(** gameState.mli *)

type t
type territory = string
type territory_list = territory list
type continent_name = string
type continent_list = continent_name list
type player_id = int

(** only players still in the game *)
val get_player_id_list : t -> player_id list

val get_territories : t -> player_id -> territory list

val get_is_human : t -> player_id -> bool

val get_name : t -> player_id -> string

val get_activite_player : t -> player_id

val get_armies : t -> territory -> int

val get_territory_owner : t -> territory -> player_id

val set_num_armies : t -> territory -> int -> t

val set_territory_owner : t -> territory -> player_id -> t

val remove_player : t -> player_id -> t

val set_next_player : t -> t
