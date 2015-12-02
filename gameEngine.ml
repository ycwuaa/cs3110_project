(** gameEngine.ml *)

open GameState

let init_game state =
  let (human, computer) = Input.choose_start () in
  let rec get_name_id id bound cur_state is_human =
    match id with
    | id when id<bound -> cur_state
    | _ -> let new_player = create_player id in
           let new_name = if is_human then Input.choose_name ()
                          else AI.choose_name ()
           in
           let new_state = add_player cur_state new_player new_name is_human in
           get_name_id (id-1) bound new_state is_human
  in
  let add_human_state = get_name_id (human+computer-1) (computer-1) state true in
  let add_all_player = get_name_id (computer-1) 0 add_human_state false in
  let army_each =
    match (human+computer) with
    | 3 -> 35
    | 4 -> 30
    | 5 -> 25
    | 6 -> 20
    | _ -> failwith "invalid number of player"
  in
  let rec get_army_distribute id cur_state =
    match id with
    | id when id > computer ->
      let list_army = Input.place_original_armies cur_state army_each in
      let after_state = Init.place_army cur_state list_army in
      get_army_distribute (id-1) after_state
    | id when id >= 0 ->
      let list_army = AI.place_original_armies cur_state army_each in
      let after_state = Init.place_army cur_state list_army in
      get_army_distribute (id-1) after_state
    | _ -> cur_state
  in
  let distribute_army_state =
    get_army_distribute (human+computer-1) add_all_player in
  Init.set_first_player distribute_army_state

(*let end_turn state =*)



(* ask the active player to place new armies at the beginning of the turn
 * parameters: a game state [gs], the active player [p'], and the number of new
 *   pieces to place [num_new_pieces]
 * returns: a new game state with these newly placed armies *)
 let rec place_all_turn gs num_new_pieces =
  let p' = get_active_player gs in
  if num_new_pieces <= 0 then
    gs
  (* if current player is AI *)
  else if not (get_is_human gs p') then
    let (num, where) = AI.place_new_armies gs num_new_pieces in
    let curr_armies = get_armies gs where in
    let _ = Printf.printf "Player %s puts %d pieces on %s \n"
      (get_name gs p') num (string_of_territory gs where) in
    let _ = Printf.printf "%d armies left\n" num_new_pieces in
    let gs = set_num_armies gs where (curr_armies + num) in
    let _ = Printf.printf "%d armies are now on %s\n"
      (get_armies gs where) (string_of_territory gs where) in
    place_all_turn gs (num_new_pieces - num)

  (* if current player is Human *)
  else
    let (num, where) = Input.place_new_armies gs num_new_pieces in
    let curr_armies = get_armies gs where in
    let _ = Printf.printf "Player %s puts %d pieces on %s \n"
      (get_name gs p') num (string_of_territory gs where) in
    let _ = Printf.printf "%d armies left\n" num_new_pieces in
    let gs = set_num_armies gs where (curr_armies + num) in
    let _ = Printf.printf "%d armies are now on %s\n"
      (get_armies gs where) (string_of_territory gs where) in

    place_all_turn gs (num_new_pieces - num)

(* ask the active player if (s)he wants to attack
 * parameters: a game state [gs] and the active player [p']
 * returns: a new game state representing the situation after the attacks are
 *   performed
 *
 * TODO: something different for human players*)
let rec do_attack gs =
  (** rolls dice n times and returns resuls in a list *)
  let rec rolls n =
    match n with
    | 0 -> []
    | _ -> Attack.roll_dice () :: (rolls (n - 1))
  in

  (** returns num dice chosen to roll - calls Input if [human] is true, AI
    * otherwise*)
  let def_dice_choice gs from toward num_dice max human =
    if human then
      Input.choose_dice gs from toward num_dice max
    else
      AI.choose_dice gs from toward num_dice max
  in

  let p' = get_active_player gs in
  if not (get_is_human gs p') then
    match (AI.choose_attack gs) with
    | None -> gs
    | Some (from, toward, num_dice) ->
      let _ = Printf.printf "Attacking from %s to %s with %d armies\n"
        (string_of_territory gs from) (string_of_territory gs toward) num_dice in

      (* determine humanity of defender *)
      let human = get_is_human gs (get_territory_owner gs toward) in
      let attacker_rolls = rolls num_dice in
      (*determine max # die defender can roll*)
      let max = Attack.max_defend gs toward in
      let num_dice_def = def_dice_choice gs from toward num_dice max human in
      let defender_rolls = rolls num_dice_def in
      let (outcome_a, outcome_d) =
        Attack.attack_outcome attacker_rolls defender_rolls in

      (* remove a piece from [from] for each defender victory *)
      let gs = Attack.remove_pieces gs outcome_d from in
      (* remove a piece from [toward] for each attacker victory *)
      let gs = Attack.remove_pieces gs outcome_a toward in

      if (Attack.is_captured gs toward) then
        let invade_with = AI.choose_move_conquerors gs from toward num_dice in
        let gs = Attack.invade gs invade_with from toward in
        do_attack gs
      else
        do_attack gs

  else
    match (Input.choose_attack gs) with
    | None -> gs
    | Some (from, toward, num_dice) ->
      let _ = Printf.printf "Attacking from %s to %s with %d armies\n"
       (string_of_territory gs from) (string_of_territory gs toward) num_dice in

      (* determine humanity of defender *)
      let human = get_is_human gs (get_territory_owner gs toward) in

      let attacker_rolls = rolls num_dice in
      (*determine max # die defender can roll*)
      let max = Attack.max_defend gs toward in
      let num_dice_def = def_dice_choice gs from toward num_dice max human in
      let defender_rolls = rolls num_dice_def in
      let (outcome_a, outcome_d) =
        Attack.attack_outcome attacker_rolls defender_rolls in

      (* remove a piece from [from] for each defender victory *)
      let gs = Attack.remove_pieces gs outcome_d from in
      (* remove a piece from [toward] for each attacker victory *)
      let gs = Attack.remove_pieces gs outcome_a toward in

      if (Attack.is_captured gs toward) then
        let invade_with = Input.choose_move_conquerors gs from toward num_dice in
        let gs = Attack.invade gs invade_with from toward in
        do_attack gs
      else
        do_attack gs

(** perform one player's turn
 * returns unit only once the game ends *)
let rec turn gs =
  let current_player = get_active_player gs in
  (* start turn *)
  let num_new_pieces = BeginTurn.award_pieces gs current_player in
  let _ = Printf.printf "Player %s gets %d new pieces\n" (get_name gs current_player) num_new_pieces in
  let gs = place_all_turn gs num_new_pieces in

  (* ask for attack *)
  let gs = do_attack gs in

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