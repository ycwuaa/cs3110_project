(** gameState.mli *)

type t
type territory = string
type territory_list = territory list
type continent_name = string
type continent_list = continent_name list
type player_id = int

(** only players still in the game *)
val get_player_id_list : t -> player_id list

(** given the player_id get back all the territories belong to the person*)
val get_territories : t -> player_id -> territory list

(** use to identify it is a computer player or human*)
val get_is_human : t -> player_id -> bool

(** get the name of the player correspnding to the given id*)
val get_name : t -> player_id -> string

(** get the current active player_id*)
val get_activite_player : t -> player_id

(** get the numbe of armies on the specified territory*)
val get_armies : t -> territory -> int

(** give back the id of the owner of the given territory*)
val get_territory_owner : t -> territory -> player_id

(** given the territory and the number of armies intended to change the number
  * on the territory to the new specified value*)
val set_num_armies : t -> territory -> int -> t

(** given the territory and the new owner's playerid to
  * set the new owner of the territory*)
val set_territory_owner : t -> territory -> player_id -> t

(** remove a player from the game by giving the playerid*)
val remove_player : t -> player_id -> t

(** set the active player to the next player*)
val set_next_player : t -> t

(** set the active to the given playerid*)
val set_active_player : t -> int -> t
