(** output.mli *)
(** Displays game information on screen to player **)

open GameState
open Graphics

let alphabet = ref []
let cur_gs = ref (new_state ())

type align = Right | Left | Center

let rec wait_for_exit () =
  let a = read_key () in
  if a <> '' then wait_for_exit () else ()

(* http://stackoverflow.com/questions/5774934/
 * how-do-i-read-in-lines-from-a-text-file-in-ocaml *)
let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let rec lookup a lst =
  match lst with
  | [] -> failwith "Not_found"
  | (b,c)::t -> if(a = b) then c else lookup a t


let string_to_color (s:string) (empty:char) (cmap:(char * color) list)=
  let clst = ref [] in
  let f c = clst := if(c = empty)
    then (None)::!clst
    else (Some (lookup c cmap))::!clst
  in
  String.iter f s; List.rev !clst

let rec ascii_to_color ascii empty cmap =
  match ascii with
  | [] -> []
  | h::t -> [string_to_color h empty cmap]@(ascii_to_color t empty cmap)

let rec plot_image img x y scale =
  let rec plot_row lst cx =
    match lst with
    | [] -> ()
    | (None)::t ->  plot_row t (cx+scale)
    | (Some h)::t ->
        (set_color h;
         fill_rect cx y scale scale;
         plot_row t (cx+scale))
  in
  match img with
  | [] -> ()
  | h::t -> (plot_row h x; plot_image t x (y-scale) scale)

let create_alphabet () =
  let ascii = read_file "alphabet.txt" in
  let s = "ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz0123456789:." in
  let rec read_letter txt n =
    match txt with
    | [] -> []
    | a::b::c::d::e::f::g::h::tl ->
        (String.get s n, [a;b;c;d;e;f;g;h])::read_letter tl (n+1)
    | _ -> failwith "Failed to load alphabet"
  in
  read_letter ascii 0

let draw_text s x y scale color a =
  let newx = match a with
    | Left -> x
    | Right -> x - (String.length s)*scale*8
    | Center -> x - (String.length s)*scale*4
  in
  let f n c =
    let img = ascii_to_color (lookup c !alphabet) '.' [('#', color)] in
    plot_image img (newx+(n*scale*8)) y scale
  in
  String.iteri f s


let draw_title () =
  let ascii = read_file "title.txt" in
  let empty = ' ' in
  let cmap = [('#',black); (':',red); ('.',rgb 255 165 0)] in
  let img = ascii_to_color ascii empty cmap in
  plot_image img 45 530 10

let clear_screen () =
  set_color white;
  fill_rect 0 0 1000 720


(******************************************************************************)
(******************************************************************************)

(** takes a message and displays it to the player e.g. "Please choose ..." *)
let draw_message s =
  set_color white;
  fill_rect 0 680 1000 40;
  draw_text s 8 684 4 black Left

(** takes a description and input string and displays it on the screen
 * e.g. draw_input_string "Enter name:" "Alice" will display
 * "Enter name: Alice" on the screen. *)
let draw_input_string s input =
  set_color white;
  fill_rect 0 0 1000 40;
  draw_text s 12 44 4 black Left;
  draw_text input (12+(String.length s)*32) 44 4 blue Left

(** takes a description and input int and displays it on the screen as in
 * draw_input_string *)
let draw_input_int s input =
  draw_input_string s (string_of_int input)

(** displays the title screen screen of the game *)
let draw_start () =
  open_graph " 1000x720+50-100";
  alphabet := (create_alphabet ());
  draw_text "CS3110 FA15" 500 160 4 black Center;
  draw_title ()

(** displays the win/lose screen screen of the game
  * *)
let draw_end id =
  clear_screen ();
  let name = get_name !cur_gs id in
  draw_text (name^" is the winner.") 500 160 4 black Center;
  draw_title ();
  draw_message "Thanks for playing.";
  draw_input_string "Press ESC to exit." ""

(** displays the world map with countries colored based on player, number of
 * armies on each country, and current player *)
and draw_map gs =
  failwith "TODO"

(** takes the game state, attacking territory, defending territory, and 2 lists
 * of dice rolls and then displays the data on the the screen to the player *)
let draw_battle gs attack defend arolls drolls =
  failwith "TODO"

(** takes a gamestate and a territory and displays the same map as in
 * redraw_map except with the territory highlighted. *)
let draw_highlight gs tert =
  failwith "TODO"



let () = draw_start ();
draw_input_int "Enter number of players:" 5;
let id = (create_player 0) in
cur_gs := add_player !cur_gs id "Tomek" true;
draw_end id;
wait_for_exit () in ()