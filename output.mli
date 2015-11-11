(** output.mli *)
(** Displays game information on screen to player **)

Open GameState

(** displays the title screen screen of the game *)
val draw_start: unit -> unit;

(** displays the win/lose screen screen of the game *)
val draw_end: player_id -> unit;

(** displays the world map with countries colored based on player, number of
 * armies on each country, and current player *)
val draw_map: t -> unit;

(** takes two player ids, two territories, two sizes of armies, and two lists
 * of dice rolls and then displays the data on the the screen to the player *)
val draw_battle: player_id -> player_id -> territory -> territory ->
  int -> int -> int list -> int list -> unit;

(** takes a gamestate and a territory and displays the same map as in
 * redraw_map except with the territory highlighted. *)
val draw_highlight: t -> territory -> unit;

(** takes a message and displays it to the player e.g. "Please choose ..." *)
val draw_message: string -> unit;

(** takes a description and input string and displays it on the screen
 * e.g. draw_input_string "Enter name:" "Alice" will display
 * "Enter name: Alice" on the screen. *)
val draw_input_string: string -> string -> unit;

(** takes a description and input int and displays it on the screen as in
 * draw_input_string *)
val draw_input_int: string -> int -> unit;
