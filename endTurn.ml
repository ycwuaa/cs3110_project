open GameState

let move_army (state:t) player army start destination =
  let start_num = get_armies state start in
  let start_t = set_num_armies state start (start_num-army) in
  let dest_num = get_armies state destination in
  let end_t = set_num_armies start_t destination (dest_num+army) in
  end_t

let check_win (state:t) =
  let players = get_player_id_list state in
  if (List.length players) = 1 then Some (List.hd players)
  else None

let check_defeated (state:t) =
  let players = get_player_id_list state in
  let rec helper = function
    | [] -> []
    | h::t -> let terri = get_territories state h in
              if (List.length terri) = 0 then h::(helper t)
              else (helper t)
  in
  helper players


