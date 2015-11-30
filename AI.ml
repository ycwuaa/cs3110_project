(** AI.mli *)

open GameState

let choose_name =
  let curr_int = ref 1 in
  fun () ->
    let returnVal = "CPU " ^ (string_of_int !curr_int) in
    let _ = incr curr_int in
    returnVal

let place_original_armies (gs:t) (num_new:int) =
  failwith "place_original_armies unimplemented"

let place_new_armies (gs:t) (num_new:int) =
  let p' = get_active_player gs in
  (* place 2/3 of current armies on the first known territory *)
  let my_terr = get_territories gs p' in
  let _ = Printf.printf "%s Territory %s\n" (string_of_territory gs (List.hd my_terr)) in
  ((if num_new > 1 then (num_new * 2 / 3) else 1), List.hd my_terr)

let choose_attack (gs:t) =
  let p' = get_active_player gs in
  if Attack.roll_dice () <= 1 then
    None
  else
    let my_terr = get_territories gs p' in
    let usable_terr = List.filter (fun terr -> get_armies gs terr > 1) my_terr in

    (*let rec print_terr_list = function
    | [] -> ()
    | h::t ->
      let _ = Printf.printf "the territory %s\n" (string_of_territory gs h) in
      print_terr_list t
    in

    let _ = print_terr_list usable_terr in*)

    match usable_terr with
    | [] -> None
    | h::t ->
        let neighbors = get_adjacency gs h in
        match neighbors with
        | [] -> None
        | h::_ -> Some (h, List.hd neighbors, 3) (* TODO: make sure it's an enemy territory *)


let choose_move_conquerors (gs:t) (terr1:territory)
  (terr2:territory) (minMove:int) =
  minMove

let redistribute_armies  (gs:t) =
  None

let choose_dice (gs:t) (terr1:territory) (terr2:territory) (num_enemy:int)
  (max_dice:int) =
  max_dice