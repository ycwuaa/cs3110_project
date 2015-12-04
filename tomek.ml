(*open Graphics

let () = Graphics.open_graph " 1000x720+50-0"

let rec wait_for_key () =
  let a = Graphics.read_key () in
  if a <> 'q' then wait_for_key ()

let _ = wait_for_key ()
*)

open GameState
open Output
open Init


let rec wait_for_exit () =
  let a = Graphics.read_key () in
  if a <> '' then wait_for_exit () else ()


let gs1 = new_state () in
let id0 = create_player 0 in
let id1 = create_player 1 in
let id2 = create_player 2 in
let id3 = create_player 3 in
let gs2 = add_player gs1 (id0) "A" true |>
fun x -> add_player x (id1) "B" true |>
fun x -> add_player x (id2) "C" true |>
fun x -> add_player x (id3) "D" true |>
fun x -> set_active_player x (id2) in
let gs = distribute_territory gs2
in
let _ = gs in
draw_start();
draw_map gs;
draw_input_string "Hello there." "skdhfsd";
draw_input_string "Bye." "shdfghjsd";
wait_for_exit ();


