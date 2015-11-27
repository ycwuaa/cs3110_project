(** output.mli *)
(** Displays game information on screen to player **)

open GameState
open Graphics

let alphabet = ref []
let cur_gs = ref (new_state ())
let player_colors = ref []
let country_coor = [
  ("Alaska",12,30);
  ("Alberta",36,43);
  ("Central America",40,81);
  ("Eastern United States",55,67);
  ("Greenland",85,22);
  ("Northwest Territory",41,29);
  ("Ontario",52,46);
  ("Quebec",70,49);
  ("Western United States",37,59);
  ("Argentina",62,142);
  ("Brazil",77,112);
  ("Peru",56,114);
  ("Venezuela",59,97);
  ("Great Britain",104,57);
  ("Iceland",105,39);
  ("Northern Europe",127,61);
  ("Scandinavia",129,34);
  ("Southern Europe",125,73);
  ("Ukraine",147,49);
  ("Western Europe",104,82);
  ("Congo",135,129);
  ("East Africa",151,120);
  ("Egypt",135,100);
  ("Madagascar",159,152);
  ("North Africa",117,110);
  ("South Africa",138,146);
  ("Afghanistan",167,66);
  ("China",196,78);
  ("India",180,90);
  ("Irkutsk",198,48);
  ("Japan",226,63);
  ("Kamchatka",219,29);
  ("Middle East",154,87);
  ("Mongolia",201,63);
  ("Siam",201,97);
  ("Siberia",180,24);
  ("Ural",169,43);
  ("Yakutsk",201,26);
  ("Eastern Australia",233,138);
  ("Indonesia",205,122);
  ("New Guinea",229,117);
  ("Western Australia",219,149)]

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
         let nscale = if(scale = 1) then 0 else scale in
         fill_rect cx y nscale nscale;
         plot_row t (cx+scale))
  in
  match img with
  | [] -> ()
  | h::t -> (plot_row h x; plot_image t x (y-scale) scale)

let create_alphabet () =
  let ascii = read_file "alphabet.txt" in
  let s = "ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz0123456789:.><="
  in
  let rec read_letter txt n =
    match txt with
    | [] -> []
    | a::b::c::d::e::f::g::h::tl ->
        (String.get s n, [a;b;c;d;e;f;g;h])::read_letter tl (n+1)
    | _ -> failwith "Failed to load alphabet"
  in
  read_letter ascii 0

let create_player_colors pids =
  let cols = [| rgb 230 81 0;
                rgb 50 105 30;
                rgb 26 35 126;
                rgb 74 20 140;
                rgb 183 28 28;
                rgb 0 96 100 |] in
  let rec colorer lst n=
    match lst with
    | [] -> []
    | h::t -> (h,cols.(n))::(colorer t (n+1))
  in (no_one,rgb 33 33 33)::(colorer pids 0)

let terr_color gs terr =
  let id = get_territory_owner gs terr in
  let (_,c) = List.find (fun (n,_) -> (n = id)) !player_colors in
  c

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
  set_color (rgb 128 128 128);
  fill_rect 0 0 1000 720

let create_ccmap gs special =
  let cs = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnop" in
  let rec add_cc lst n =
    match lst with
    | [] -> []
    | (h,_,_)::t ->
      let col = terr_color gs h in
      let ncol = if(h = special) then col*2 else col in
    ((String.get cs n),ncol)::(add_cc t (n+1))
  in
  add_cc country_coor 0

let draw_map_info gs =
  let rec drawer lst =
    match lst with
    | [] -> ()
    | (n,x,y)::t ->
        draw_text n (x*4) (720-(y*4)+9) 1 white Center;
        let army = string_of_int (get_armies gs n) in
        draw_text army (x*4) (720-(y*4)-3) 1 white Center;
        drawer t
  in drawer country_coor

(******************************************************************************)
(******************************************************************************)

(** takes a message and displays it to the player e.g. "Please choose ..." *)
let draw_message s =
  set_color (rgb 128 128 128);
  fill_rect 0 684 1000 36;
  draw_text s 8 712 3 black Left

(** takes a description and input string and displays it on the screen
 * e.g. draw_input_string "Enter name:" "Alice" will display
 * "Enter name: Alice" on the screen. *)
let draw_input_string s input =
  set_color (rgb 128 128 128);
  fill_rect 0 0 1000 40;
  draw_text s 12 36 3 black Left;
  draw_text input (12+(String.length s)*24) 36 3 blue Left

(** takes a description and input int and displays it on the screen as in
 * draw_input_string *)
let draw_input_int s input =
  draw_input_string s (string_of_int input)

(** displays the title screen screen of the game *)
let draw_start () =
  open_graph " 1000x720+50-100";
  alphabet := (create_alphabet ());
  clear_screen ();
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
let draw_map gs =
  if(!player_colors = [])
    then player_colors := create_player_colors (get_player_id_list gs) else ();
  clear_screen ();
  let ascii = read_file "world.txt" in
  let empty = '.' in
  let cmap = ('#',black)::(create_ccmap gs "") in
  let img = ascii_to_color ascii empty cmap in
  plot_image img 0 720 4;
  draw_map_info gs



(** takes the game state, attacking territory, defending territory, and 2 lists
 * of dice rolls and then displays the data on the the screen to the player *)
let draw_battle gs attack defend arolls drolls =
  failwith "TODO"

(** takes a gamestate and a territory and displays the same map as in
 * redraw_map except with the territory highlighted. *)
let draw_highlight gs terr =
  if(!player_colors = [])
    then player_colors := create_player_colors (get_player_id_list gs) else ();
  clear_screen ();
  let ascii = read_file "world.txt" in
  let empty = '.' in
  let cmap = ('#',black)::(create_ccmap gs terr) in
  let img = ascii_to_color ascii empty cmap in
  plot_image img 0 720 4;
  draw_map_info gs



