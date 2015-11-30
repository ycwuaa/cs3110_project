(** gameEngine.ml *)

open GameState

(* ask the active player to place new armies at the beginning of the turn
 * parameters: a game state [gs], the active player [p'], and the number of new
 *   pieces to place [num_new_pieces]
 * returns: a new game state with these newly placed armies
 *
 * TODO: remove p' as a parameter, as it can be read from gs*)
let rec place_all_turn gs p' num_new_pieces =
  if num_new_pieces <= 0 then
    gs
  else
    let (num, where) = AI.place_new_armies gs num_new_pieces in
    let curr_armies = get_armies gs where in
    let _ = Printf.printf "Player %s puts %d pieces on %s \n" (get_name gs p') num (string_of_territory gs where) in
    let _ = Printf.printf "%d armies left\n" num_new_pieces in
    let gs = set_num_armies gs where (curr_armies + num) in
    let _ = Printf.printf "%d armies are now on %s\n" (get_armies gs where) (string_of_territory gs where) in



    place_all_turn gs p' (num_new_pieces - num)

(* ask the active player if (s)he wants to attack
 * parameters: a game state [gs] and the active player [p']
 * returns: a new game state representing the situation after the attacks are
 *   performed
 *
 * TODO: remove p' as a parameter, as it can be read from gs *)
let rec do_attack gs p' =
  match (AI.choose_attack gs) with
  | None -> gs
  | Some (from, toward, num_dice) ->
    let _ = Printf.printf "Attacking from %s to %s with %d armies\n" (string_of_territory gs from) (string_of_territory gs toward) num_dice in
    let gs = gs in (* TODO: actually update the gs *)
    do_attack gs p'

(** perform one player's turn
 * returns unit only once the game ends *)
let rec turn gs =
  let current_player = get_active_player gs in
  (* start turn *)
  let num_new_pieces = BeginTurn.award_pieces gs current_player in
  let _ = Printf.printf "Player %s gets %d new pieces\n" (get_name gs current_player) num_new_pieces in
  let gs = place_all_turn gs current_player num_new_pieces in

  (* ask for attack *)
  let gs = do_attack gs current_player in

  (* end turn *)
  (* TODO *)

  (* recurse
   * TODO: increment current player *)
  turn gs

(** the main function; calling this runs the program
 *  written physically in this function is initialization code;
 *  the turns are handled in a call to [turn] (which recurses on itself) *)
let main () =
  try
    let gs = new_state () in

    (* initialize game state
     * TODO: this is all just a stub*)
    let player0 = create_player 0 in
    let gs = GameState.add_player gs player0 (AI.choose_name () ) false in
    let gs = Init.set_first_player gs in
    (* TODO: should create_player just take in a unit? *)

    turn gs

    (* end game, or restart main if play again *)
    (* TODO*)
  with
  | e ->
    print_endline "Oh no! Something failed! Quitting now.";
    raise e (* TODO: remove the reraise *)

let _ = main ()