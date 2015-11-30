(** gameEngine.ml *)

open GameState

(* keep ask to place pieces at beginning of the turn until run out *)
let rec place_all gs p' num_new_pieces =
  if num_new_pieces <= 0 then
    gs
  else
    let (num, where) = AI.place_new_armies gs p' num_new_pieces in
    let curr_armies = get_armies gs where in
    let _ = Printf.printf "Player %s puts %d pieces on %s \n" (get_name gs p') num (string_of_territory gs where) in
    let _ = Printf.printf "%d armies left\n" num_new_pieces in
    let gs = set_num_armies gs where (curr_armies + num) in
    let _ = Printf.printf "%d armies are now on %s\n" (get_armies gs where) (string_of_territory gs where) in



    place_all gs p' (num_new_pieces - num)

(* keep ask to attack until None *)
let rec do_attack gs p' =
  match (AI.choose_attack gs p') with
  | None -> gs
  | Some (from, toward, num_dice) ->
    let _ = Printf.printf "Attacking from %s to %s with %d armies\n" (string_of_territory gs from) (string_of_territory gs toward) num_dice in
    let gs = gs in (* TODO: actually update the gs *)
    do_attack gs p'

(** perform 1 player's turn *)
let rec turn gs =
  let current_player = get_active_player gs in
  (* start turn *)
  let num_new_pieces = BeginTurn.award_pieces gs current_player in
  let _ = Printf.printf "Player %s gets %d new pieces\n" (get_name gs current_player) num_new_pieces in
  let gs = place_all gs current_player num_new_pieces in

  (* ask for attack *)
  let gs = do_attack gs current_player in

  (* end turn *)

  (* recurse *)
  ignore gs;
  ()

(** the main function; calling this runs the program
 *  written physically in this function is initialization code;
 *  the turns are handled in a call to [turn] (which recurses on itself *)
let main () =
  try
    let gs = new_state () in

    let player0 = create_player 0 in
    let gs = GameState.add_player gs player0 (AI.choose_name () ) false in
    let player1 = create_player 1 in
    let gs = GameState.add_player gs player1 (AI.choose_name () ) false in
    let gs = Init.set_first_player gs in
    (* TODO: should create_player just take in a unit? *)
    turn gs
  with
  | e ->
    print_endline "Oh no! Something failed! Quitting now.";
    raise e (* TODO: remove the reraise *)

let _ = main ()