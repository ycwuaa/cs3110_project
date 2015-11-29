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

let attacker_rolls = [0; 0; 0; 0; 0]
let defender_rolls = [1; 2; 3; 4; 5]
let (awins, dwins) = attack_outcome attacker_rolls defender_rolls

TEST_UNIT "attack_outcome" = awins === 0

TEST_UNIT "attack_outcome" = dwins === 5

let attacker_rolls = [0; 0; 0; 0]

TEST_UNIT "attack_outcome" = awins === 0

TEST_UNIT "attack_outcome" = dwins === 4