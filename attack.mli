(** attack.mli ****************************************************************)
(** Handles checking if a desired attack is valid and performing the  attack **)
(** mechanic ******************************************************************)

Open GameState


(** returns a pseudo-random int between 1 and 6 (inclusive)*)
val roll_dice: () -> int

(** returns a bool  that represents the outcome of a dice match-up that is true
  * if attacker won and false otherwise *)
val attack_outcome: int -> int -> bool

(** updates the map by removing pieces from the territory  in the case where
  * attacker wins the dice roll; returns an updated game state *)
val perform_attack: player_id -> player_id -> territory -> t

(** checks if a territory is captured after an attack; returns true if all
 ** pieces have been removed from the territory, false otherwise *)
val is_captured: territory -> bool

(** updates the game state by moving n pieces of player_id onto a newly captured
  * territory *)
val invade: t -> int -> player_id -> territory -> t

(** returns true if the given territory is adjacent to a territory controlled by
  * player_id in the game state, false otherwise*)
val check_adjacent: t -> player_id -> territory -> bool

(** returns true if player_id has at least two pieces on the given territory in
  * the game state, false otherwise*)
val check_min_pieces: t -> player_id -> territory -> bool