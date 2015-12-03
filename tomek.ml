(*open Graphics

let () = Graphics.open_graph " 1000x720+50-0"

let rec wait_for_key () =
  let a = Graphics.read_key () in
  if a <> 'q' then wait_for_key ()

let _ = wait_for_key ()
*)

open GameState
open Output

(* http://stackoverflow.com/questions/15095541/
 * how-to-shuffle-list-in-on-in-ocaml *)
let shuffle d =
    let () = Random.self_init () in
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond

let distribute_territory (gs:t) =
  let terrs = shuffle (get_all_territories gs) in
  let cur = get_active_player gs in
  let give ngs terr =
    let p = get_active_player ngs in
    set_territory_owner ngs terr p |>
    fun x -> set_num_armies x terr 1 |>
    set_next_player
  in
  (List.fold_left give gs terrs) |>
  fun x -> set_active_player x cur

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
wait_for_exit ();


