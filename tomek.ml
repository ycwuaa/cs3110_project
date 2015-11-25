(*open Graphics

let () = Graphics.open_graph " 1000x720+50-0"

let rec wait_for_key () =
  let a = Graphics.read_key () in
  if a <> 'q' then wait_for_key ()

let _ = wait_for_key ()
*)

open Output
draw_start ()