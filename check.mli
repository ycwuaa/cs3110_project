(** check.mli*)

open GameState

(** the function takes in [current state] and intended player's info [is_human]
  * and [player name]. the new state after adding the player needs to satisfy :
  * 1. no. of human players >=0
  * 2. no. of AI players >=0
  * 3. no. of total players <=6
  * 4. name is not empty*)
val check_add_player : t -> bool -> string -> bool

(** check that human players >=1, total players >=3*)
val check_all_players : t -> bool

(** use for init stage. use [current state] to figure out how many players in
  * the game. and take in the [army for each territory] to see if the sum
  * exceeds the total distribute to the player*)
val check_army_enough : t -> int list -> bool

(** take in [current state] and compare whether the [territory specified]
  * has [enough army] to be moved*)
val check_move_army : t -> territory -> int -> bool

(** check whether the [territory specifed] belongs to the [player]*)
val check_belong_to : t -> territory -> player_id -> bool

(** give two territory and give back whether they are adjacent*)
val check_adjacent : t -> territory -> territory -> bool