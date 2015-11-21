(** attack.mli ****************************************************************)
(** Handles checking if a desired attack is valid and performing the  attack **)
(** mechanic ******************************************************************)

open GameState
open Random


(** returns a pseudo-random int between 1 and 6 (inclusive)*)
let roll_dice () : int =
  (*get number between 0 and 5 (inclusive)*)
  let result = int 6 in
  (*add 1 to result and return*)
  result + 1

(** takes in two sorted lists representing the outcomes of attckers and defender
  * die rolls; returns a pair of ints represnting the number or successes for
  * attacker and defender respectively*)
let attack_outcome (attacker: int list) (defender: int list) : int*int =
  (*takes in two int lists representing die rolls of each player and accumulates
  score of each by comparing each int*)
  let rec compare d1 d2 s1 s2 =
    match d1 with
    | [] -> (s1, s2)
    | hd1 :: tl1 -> (
      match d2 with
      | [] -> compare tl1 [] (s1 + 1) s2
      | hd2 :: tl2 ->
        if hd1 > hd2 then compare tl1 tl2 (s1+1) s2
        else compare tl1 tl2 s1 (s2 + 1))
  in
  compare attacker defender 0 0

(** returns and updated gamestate with n pieces removed from the given
  * territory *)
let remove_pieces (state: t) (n: int) (terr: territory): t =
  (*get current number on territory*)
  let current = get_armies state terr in
  (*subtract n pieces and return updated state*)
  let new_num = current - n in
  set_num_armies state terr new_num

(** checks if a territory is captured after an attack; returns true if all
 ** pieces have been removed from the territory, false otherwise *)
let is_captured (state: t) (terr: territory): bool =
  let num = get_armies state terr in
  if num = 0 then true else false

(** updates the game state by moving n pieces of allied with the owner of the
  * first territory to the second territory *)
let invade (state: t) (n: int) (terr1: territory) (terr2: territory): t =
  (*get terr1 owner pid*)
  let player = get_territory_owner state terr1 in
  (*update territory owner*)
  let state1 = set_territory_owner state terr2 player in
  (*remove n pieces from terr1*)
  let current = get_armies state1 terr1 in
  let new_num = current - n in
  let state2 = set_num_armies state1 terr1 new_num in
  (*add n peices to terr 2 and return updated gamestate*)
  let current = get_armies state2 terr2 in
  let new_num = current + n in
  set_num_armies state2 terr2 new_num

(** returns true if there are enough peices in territory for its owner to
  * perform an attack with n pieces (need to have n+1 pieces in territory)*)
let check_min_pieces (state: t)  (n: int) (terr: territory): bool =
  (*TODO: move to checking*)
  let num_pieces = get_armies state terr in
  if n >= num_pieces then false else true

