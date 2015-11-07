(** attack.mli ****************************************************************)
(** Handles checking if a desired attack is valid and performing the  attack **)
(** mechanic *)

Open GameState


(** returns a pseudo-random int between 1 and 6 (inclusive)*)
val roll_dice: () -> int

(** returns a bool  that represents the outcome of a dice match-up that is true
  * if attacker won and false otherwise *)
val attack_outcome: int -> int -> bool

(** updates the map by removing pieces from the territory  in the case where
  * attacker wins the dice roll; returns an updated territory list *)
val perform_attack: player_id -> player_id -> territory -> territory_list

(** checks if a territory is captured after an attack; returns true if all
 ** pieces have been removed from the territory, false otherwise *)
val is_captured: territory -> bool

(** updates the map by moving n pieces of player_id onto a newly captured
  * territory *)
val invade: int -> player_id -> territory -> territory_list -> territory_list

(** returns true if the given territory is adjacent to a territory controlled by
  * player_id in territory_list, false otherwise*)
val check_adjacent: player_id -> territory -> territory_list -> bool

(** returns true if player_id has at least two pieces on the given territory in
  * territory_list, false otherwise*)
val check_min_pieces: player_id -> territory -> territory_list -> bool