(** gameState.mli *)

type t
type territory
type continent
type player_id

(** a player_id representing the lack of a player *)
val no_one : player_id

(** creates a player id with given value *)
val create_player : int -> player_id

(** returns a new state with player_id added to players *)
val add_player : t -> player_id -> string -> bool -> t

(** creates a new instance of state with all territories initially unowned
  * (currently hardcoded) *)
val new_state : unit -> t

(** only players still in the game *)
val get_player_id_list : t -> player_id list

(** given the player_id get back all the territories belong to the person*)
val get_territories : t -> player_id -> territory list

(** use to identify it is a computer player or human*)
val get_is_human : t -> player_id -> bool

(** get the name of the player corresponding to the given id*)
val get_name : t -> player_id -> string

(** get the current active player_id*)
val get_active_player : t -> player_id

(** get the number of armies on the specified territory*)
val get_armies : t -> territory -> int

(** give back the id of the owner of the given territory*)
val get_territory_owner : t -> territory -> player_id

(** get a list of continents controlled by player_id, returns [] if no continents are held *)
val get_continents : t -> player_id -> continent list

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
val set_active_player : t -> player_id -> t

(** return the name of the given territory *)
val string_of_territory : t -> territory -> string

(** return the name of the given continent *)
val string_of_continent : t -> continent -> string

(** returns true if the two territories are adjacent *)
val get_adjacency : t -> territory -> territory list