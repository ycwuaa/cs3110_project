open GameState
open Assertions
open Attack

let dice = roll_dice ()

TEST_UNIT "roll_dice" = assert(dice > 0 && dice < 7)

let dice = roll_dice ()

TEST_UNIT "roll_dice" = assert(dice > 0 && dice < 7)

let dice = roll_dice ()

TEST_UNIT "roll_dice" = assert(dice > 0 && dice < 7)

(* calls roll_dice n times and returns the result in a list *)
let roll_n n =
  let rec helper x lst =
    if x < 1 then lst
    else helper (x - 1) ((roll_dice ()) :: lst )
  in
  helper n []

(* returns true if every element in [lst] is < 7 and > 0 *)
let rec check_rolls lst =
  match lst with
  | [] -> true
  | hd :: tl -> hd > 0 && hd < 7 && (check_rolls tl)

let rolls = roll_n 100

TEST_UNIT = assert(check_rolls rolls)

let attacker_rolls = [1; 1; 1;]
let defender_rolls = [2; 2; 3; 4; 5]
let (awins, dwins) = attack_outcome attacker_rolls defender_rolls

TEST_UNIT "attack_outcome" = awins === 0

TEST_UNIT "attack_outcome" = dwins === 3

let attacker_rolls = [4; 5; 6]
let (awins, dwins) = attack_outcome attacker_rolls defender_rolls

TEST_UNIT "attack_outcome" = awins === 3

TEST_UNIT "attack_outcome" = dwins === 0

let attacker_rolls = [3; 4; 5]
let (awins, dwins) = attack_outcome attacker_rolls defender_rolls

TEST_UNIT "attack_outcome" = awins === 0

TEST_UNIT "attack_outcome" = dwins === 3

let attacker_rolls = [3; 4; 6]
let (awins, dwins) = attack_outcome attacker_rolls defender_rolls

TEST_UNIT "attack_outcome" = awins === 1

TEST_UNIT "attack_outcome" = dwins === 2

let state = new_state ()
let terr1 = List.hd (get_territories state no_one)
let current = get_armies state terr1
let state = remove_pieces state 0 terr1

TEST_UNIT = get_armies state terr1 === current

let state = remove_pieces state 2 terr1

TEST_UNIT "remove_pieces" = get_armies state terr1 === current - 2