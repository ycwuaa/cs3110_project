(** beginTurn.mli : ***********************************************************)
(** handles distributing army pieces and placing them on the map **************)

Open GameState

(*A turn beginning has two parts:
` 1. getting armies
    a. a player always recieves at least 3 armies
    b. # armies recieved = # territories held/ 3 (ignore remainder)
  2. placing armies
    a. armies are placed by player into territories you currently control*)


(** returns number of pieces to award to player with player_id who has control
  * of territories in territory_list; checks for continent control and awards
  * accordingly*)
val award_pieces: player_id -> territory_list -> int;

(** returns a territory list with a piece aligned with player_id placed on given
  * territory*)
val place_piece: player_id -> territory_list -> territory -> territory_list;