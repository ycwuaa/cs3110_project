(** input.mli *)
(** Handles all game actions by human players*)

open GameState
open Graphics
open Output
open Check

type battle = Yes | No | Repeat
let cur_gs = ref (new_state ())
let world_map = ref [||]
let prev_battle = ref None

let tarray = [|
  "Alaska";
  "Alberta";
  "Central America";
  "Eastern United States";
  "Greenland";
  "Northwest Territory";
  "Ontario";
  "Quebec";
  "Western United States";
  "Argentina";
  "Brazil";
  "Peru";
  "Venezuela";
  "Great Britain";
  "Iceland";
  "Northern Europe";
  "Scandinavia";
  "Southern Europe";
  "Ukraine";
  "Western Europe";
  "Congo";
  "East Africa";
  "Egypt";
  "Madagascar";
  "North Africa";
  "South Africa";
  "Afghanistan";
  "China";
  "India";
  "Irkutsk";
  "Japan";
  "Kamchatka";
  "Middle East";
  "Mongolia";
  "Siam";
  "Siberia";
  "Ural";
  "Yakutsk";
  "Eastern Australia";
  "Indonesia";
  "New Guinea";
  "Western Australia" |]

(* https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=179199 *)
let delay n =
  let start = Unix.gettimeofday() in
  let rec mysleep t =
    try
      ignore (Unix.select [] [] [] t)
    with Unix.Unix_error(Unix.EINTR, _, _) ->
      let now = Unix.gettimeofday() in
      let remaining = start +. n -. now in
      if remaining > 0.0 then mysleep remaining in
  mysleep n

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

let string_to_terr (s:string) (tmap:(char * territory) list)=
  let clst = ref [] in
  let f c = clst := if(c = '.' || c = '#')
    then (None)::!clst
    else (Some (lookup c tmap))::!clst
  in
  String.iter f s; List.rev !clst

let rec ascii_to_terr ascii tmap =
  match ascii with
  | [] -> []
  | h::t -> [string_to_terr h tmap]@(ascii_to_terr t tmap)

let create_tmap () =
  let cs = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnop" in
  let cref = ref [] in
  let f i c = cref := (c,tarray.(i))::!cref in
  String.iteri f cs; !cref

let init_world_map () =
  let ascii = read_file "world.txt" in
  let world_list = ascii_to_terr ascii (create_tmap ()) in
  let f x = Array.of_list x in
  world_map := Array.of_list (List.map f world_list)

let rec get_int txt min max =
  let valid = "1234567890" in
  let rec loop c s =
    if(c = '\r') then
      let i =
        let slen = if(String.length s > 8) then 8 else String.length s in
      int_of_string (String.sub s 0 slen) in
      if(s <> "" && i <= max && i >= min) then i
      else
      let err = "Please enter a valid number: "^
        (string_of_int min)^"<=n<="^(string_of_int max) in
      let () = draw_message err in get_int txt min max
    else if(c = '\b' && s <> "") then
      let ns = String.sub s 0 ((String.length s)-1) in
        let () = draw_input_string txt ns in
        loop (read_key ()) ns
      else
      let ns = if(String.contains valid c) then s^(Char.escaped c) else s in
      let () = draw_input_string txt ns in
      loop (read_key ()) ns
  in loop ' ' ""

let rec get_string txt =
  let valid = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz " in
  let rec loop c s =
    if(c = '\r') then
      if(s <> "") then s
      else let () = draw_message "Please enter a valid input." in
        get_string txt
    else if(c = '\b' && s <> "") then
        let ns = String.sub s 0 ((String.length s)-1) in
        let () = draw_input_string txt ns in
        loop (read_key ()) ns
      else
        let ns = if(String.contains valid c && (String.length s) < 10)
          then s^(Char.escaped c) else s in
        let () = draw_input_string txt ns in
        loop (read_key ()) ns
  in loop '.' ""

let mouse_press () =
  let rec mouse_loop b =
    if(b) then mouse_pos () else mouse_loop (button_down ())
  in mouse_loop false

let choose_territory gs b =
  let () = draw_input_string "Select a territory." "" in
    let rec terr_loop () =
      let (x,y) = mouse_press () in
      let oterr = (!world_map).((720-y)/4).(x/4) in
      match oterr with
      | None -> None
      | Some x ->
          let terr = x in
          let owner = get_territory_owner gs terr in
          let active = get_active_player gs in
          if((owner <> active && b)||(owner = active && (not b)))
            then
              let () = draw_message (terr^" is invalid.") in
              let () = delay 0.1 in
              terr_loop ()
            else Some terr
    in terr_loop ()

let get_ynr () =
  let rec loop c =
    match c with
    | 'y' -> Yes
    | 'n' -> No
    | 'r' -> Repeat
    | _ -> loop (read_key ())
  in loop ' '

let get_yn () =
  let rec loop c =
    match c with
    | 'y' -> Yes
    | 'n' -> No
    | _ -> loop (read_key ())
  in loop ' '


(** Gets the start configuration for the game (# of human, # of AI) *)
let rec choose_start () =
  let () = init_world_map () in
  let () = draw_message "Max players = 6." in
  let humans = get_int "Enter number of humans:" 1 6 in
  let () = draw_message "Max players = 6." in
  let ais = get_int "Enter number of AI:" 0 (6-humans) in
  if(humans + ais >= 3)
  then (humans, ais)
  else choose_start ()

(** Returns whether the player wants to play again. *)
let play_again () =
  let rec wait_for_exit () =
    let a = read_key () in
    if a <> '' then wait_for_exit () else false
  in wait_for_exit ()

(** Gets the player name at the beginning of the game.*)
let choose_name () =
  get_string "Please enter your name:"

(** at the beginning of the game, receive a few additional armies; return
 * a list of where to put these extra armies *)
let place_original_armies gs num =
  let rec loop n lst lgs =
    if(n = 0) then lst else
    let () = draw_map lgs in
    let () = draw_message ("Total armies remaining: "^(string_of_int n)) in
    let terro = choose_territory gs true in
    match terro with
    | None -> loop n lst lgs
    | Some terr ->
        let () = draw_highlight lgs terr in
        let () = draw_message ("Total armies remaining: "^(string_of_int n)) in
        let army = get_int "Enter number of armies to place: " 0 n in
        let ngs = set_num_armies lgs terr ((get_armies lgs terr)+army) in
        loop (n-army) ((army,terr)::lst) ngs
  in loop num [] gs

(** takes in a GameState and the number of extra armies received, and returns
 * a territory and int containing the newly added armies that the human had
 * added *)
let rec place_new_armies gs num =
  let () = draw_map gs in
  let () = draw_message ("Total armies remaining: "^(string_of_int num)) in
  let terro = choose_territory gs true in
  match terro with
  | None -> place_new_armies gs num
  | Some terr ->
      let () = draw_highlight gs terr in
      let () = draw_message ("Total armies remaining: "^(string_of_int num)) in
      let army = get_int "Enter number of armies to place: " 0 num in
      (army,terr)

(** takes in a GameState and returns Some (from, to, dice) if player will
 * attack the [to] territory with the army on the [from] territory using [dice]
 * number of dice, or None if no attack is desired *)
let rec choose_attack gs =
  let () = draw_map gs in
  let () = draw_message "Would you like to attack" in
  let () = draw_input_string "Y:Yes N:No R:Repeat previous battle" "" in
  match get_ynr () with
  | No -> None
  | Yes ->
      let rec loop () =
        let afromo = choose_territory gs true in
        match afromo with
        | None -> choose_attack gs
        | Some afrom ->
            let narmy = get_armies gs afrom in
            if(narmy < 2) then
              let () = draw_message (afrom^" has <2 armies.") in
              let () = delay 0.1 in loop ()
            else
            let () = draw_message ("Attacking from "^afrom) in
            let () = draw_highlight gs afrom in
            let atoo = choose_territory gs false in
            match atoo with
            | None -> choose_attack gs
            | Some ato ->
                if(not (check_adjacent gs afrom ato)) then
                  let () = draw_message (afrom^" is not adjacent to "^ato) in
                  let () = delay 0.1 in loop ()
                else
                let max_dice = (match (narmy=2,narmy>=4) with
                  | (true,false) -> 1
                  | (false,true) -> 2
                  | (false,false) -> 3
                  | (true,true) -> failwith "Invalid dice configuration.")
                in
                let () = draw_message ("Attacking from "^afrom^" to "^ato) in
                let () = draw_highlight gs ato in
                let dice = get_int "Enter number of dice: " 1 max_dice in
                let new_battle = Some (afrom, ato, dice) in
                let () = prev_battle := new_battle in
                new_battle
      in loop ()
  | Repeat ->
      (match !prev_battle with
      | None -> let () = draw_message "Cannot repeat previous battle" in
                let () = delay 0.5 in choose_attack gs
      | Some (a,b,c) ->
          let narmy = get_armies gs a in
          let check1 = (narmy >= 2) in
          let check2 = (get_territory_owner gs b <> get_active_player gs) in
          let check3 = (c <= (narmy-1)) in
          match (check1,check2,check3) with
          | (true,true,true) -> Some(a,b,c)
          | _ -> let () = draw_message "Cannot repeat previous battle" in
                 let () = delay 0.5 in
                 let () = prev_battle := None in choose_attack gs)

(** takes in a GameState, a territory to move from, a territory to move to, and
 * the minimum number of armies to move; returns the chosen number of armies to
 * move *)
let choose_move_conquerors gs tfrom tto min =
  let () = draw_map gs in
  let () = draw_message ("Moving armies from "^tfrom^" to "^tto^".") in
  get_int "How many do you move: " min ((get_armies gs tfrom)-1)
  (*Shouldn't this be maximum*)

(** takes in a GameState and returns Some (from, to, num) if player moves
 * [num] armies from the [from] territory to the [to] territory, or None if
 * no redistribution is desired *)
let rec redistribute_armies gs =
  let () = draw_map gs in
  let () = draw_message "Will you redistribute armies" in
  let () = draw_input_string "Y:Yes N:No" "" in
  match get_yn () with
  | No -> None
  | Yes ->
    let rec loop () =
      let afromo = choose_territory gs true in
      match afromo with
      | None -> redistribute_armies gs
      | Some afrom ->
          let narmy = get_armies gs afrom in
          if(narmy < 2) then
            let () = draw_message (afrom^" has <2 armies.") in
            let () = delay 0.1 in loop ()
          else
          let () = draw_message ("Attacking from "^afrom) in
          let () = draw_highlight gs afrom in
          let atoo = choose_territory gs true in
          match atoo with
          | None -> redistribute_armies gs
          | Some ato ->
              let army = choose_move_conquerors gs afrom ato 0 in
              Some (afrom, ato, army)
    in loop ()
  | Repeat -> failwith "Should not repeat."


(** takes in a GameState, a territory attacking from, a territory being
 * attacked, and the number of dice enemy is using and maximum number of dice
 * the player can defend with and returns the chosen number of dice *)
let choose_dice gs tfrom tto dice maxn =
  let () = draw_map gs in
  let () = draw_message (tto^" being attacked by "^tfrom) in
  let sdice = (string_of_int dice) in
  get_int ("Opp. uses "^sdice^". Choose dice to defend.") 1 maxn


(*Debug code
let () = draw_start ();
let id0 = (create_player 0) in
let id1 = (create_player 1) in
let id2 = (create_player 2) in
let id3 = (create_player 3) in
let id4 = (create_player 4) in
let id5 = (create_player 5) in
cur_gs := add_player !cur_gs id0 "TomekMarek" true;
cur_gs := add_player !cur_gs id1 "Max" true;
cur_gs := add_player !cur_gs id2 "Emily" true;
cur_gs := add_player !cur_gs id3 "Kim" true;
cur_gs := add_player !cur_gs id4 "Mark" true;
cur_gs := add_player !cur_gs id5 "Joseph" true;
cur_gs := set_territory_owner !cur_gs "Eastern United States" id0;
cur_gs := set_territory_owner !cur_gs "Japan" id0;
cur_gs := set_territory_owner !cur_gs "Western United States" id0;
cur_gs := set_territory_owner !cur_gs "Alberta" id3;
cur_gs := set_territory_owner !cur_gs "Siberia" id4;
cur_gs := set_territory_owner !cur_gs "Central America" id0;
ignore(choose_start ());
cur_gs := set_active_player !cur_gs id0;
cur_gs := set_num_armies !cur_gs "Western United States" 10;
cur_gs := set_num_armies !cur_gs "Japan" 1;
ignore(draw_battle !cur_gs "Western United States" "Alberta" [5;3;4] [6;2]);
ignore(choose_attack !cur_gs);
ignore(choose_move_conquerors !cur_gs "Western United States" "Alberta" 1);
cur_gs := set_num_armies !cur_gs "Western United States" 1;
ignore(choose_attack !cur_gs);
ignore(choose_attack !cur_gs);
()
*)