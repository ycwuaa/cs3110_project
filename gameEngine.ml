(** gameEngine.ml *)

open GameState

(** perform 1 player's turn *)
let rec turn gs =
  (*let current_player = get_active_player gs in*)
  (* start turn *)
  (*let num_new_pieces = Init.award_pieces gs current_player in*)

  (*ignore (turn gs, ())*)
  ()

(** the main loop; calling this runs the program *)
let main () =
  let gs = new_state () in
  try
    turn gs
  with
  | e ->
    print_endline "Oh no! Something failed! Quitting now.";
    raise e

let _ = main ()