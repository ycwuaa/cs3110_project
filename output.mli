(** output.mli *)
(** Displays game information on screen to player **)

open GameState

(** displays the title screen screen of the game *)
val draw_start: unit -> unit

(** displays the win/lose screen screen of the game
  * *)
val draw_end: t -> player_id -> unit

(** displays the world map with countries colored based on player, number of
 * armies on each country, and current player *)
val draw_map: t -> unit

(** takes the game state, attacking territory, defending territory, and 2 lists
 * of dice rolls and number of pieces lost, and then displays the data on the
 * the screen to the player *)
val draw_battle: t -> territory -> territory -> int list -> int list -> int*int
  -> unit

(** takes a gamestate and a territory and displays the same map as in
 * redraw_map except with the territory highlighted. *)
val draw_highlight: t -> territory -> unit

(** takes a message and displays it to the player e.g. "Please choose ..." *)
val draw_message: string -> unit

(** takes a description and input string and displays it on the screen
 * e.g. draw_input_string "Enter name:" "Alice" will display
 * "Enter name: Alice" on the screen. *)
val draw_input_string: string -> string -> unit

(** takes a description and input int and displays it on the screen as in
 * draw_input_string *)
val draw_input_int: string -> int -> unit

(** takes in a game state  attacker's id and defender's id and displays the
 * names on the map for player to see *)
val draw_attack_info: t -> territory -> territory -> unit
