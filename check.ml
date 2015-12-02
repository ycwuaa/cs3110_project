open GameState

let check_add_player (state:t) is_human name =
  let players = get_player_id_list state in
  let human = ref 0 in
  let computer = ref 0 in
  let rec helper = function
    | [] -> ()
    | h::t -> if (get_is_human state h) then human:=!human+1
              else computer:=!computer+1;helper t
  in
  helper players;
  if is_human then human:=!human+1 else computer:=!computer+1;
  if (!human>=0 && !computer>=0 && ((!human)+(!computer))<=6) then true
  else false

let check_all_players (state:t) =
  let players = get_player_id_list state in
  let human = ref 0 in
  let computer = ref 0 in
  let rec helper = function
    | [] -> ()
    | h::t -> if (get_is_human state h) then human:=!human+1
              else computer:=!computer+1;helper t
  in
  helper players;
  if (!human>=1 && ((!human)+(!computer))>=3) then true
  else false

let check_army_enough (state:t) armies_l =
  let sum_up = List.fold_left (fun a b -> a+b) 0 armies_l in
  let players = get_player_id_list state in
  let bound = match (List.length players) with
              | 3 -> 35
              | 4 -> 30
              | 5 -> 25
              | 6 -> 20
              | _ -> failwith "invalid player number"
  in
  if sum_up>bound then false
  else true

let check_move_army (state:t) terri num =
  let upper = get_armies state terri in
  if (num+1)>upper then false
  else true

let check_belong_to (state:t) terri player =
  let owner = get_territory_owner state terri in
  if player=owner then true
  else false

let check_adjacent (state:t) terri1 terri2 =
  let adj = get_adjacency state terri1 in
  List.mem terri2 adj
