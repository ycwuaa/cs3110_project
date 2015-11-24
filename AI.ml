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
  failwith "place_new_armies unimplemented"

let choose_attack (gs:t) =
  failwith "choose_attack unimplemented"

let choose_move_conquerors (gs:t) (terr1:territory) (terr2:territory)
  (minMove:int) =
  failwith "choose_move_conquerors unimplemented"

let redistribute_armies  (gs:t) =
  failwith "redistribute_armies unimplemented"