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

let state = set_num_armies state terr1 0

TEST_UNIT "is_captured" = is_captured state terr1 ===  true


let state = set_num_armies state terr1 1

TEST_UNIT "is_captured" = is_captured state terr1 ===  false

let state = set_num_armies state terr1 (-1)

TEST_UNIT "is_captured" = is_captured state terr1 ===  true

let p1 = create_player 1
let state = add_player state p1 "test player1" true
let state = set_territory_owner state terr1 p1
let terr2 = List.hd (get_territories state no_one)

let state = set_num_armies state terr1 (0)
let state = set_num_armies state terr1 (0)

let current1 = get_armies state terr1
let current2 = get_armies state terr2
let state = invade state 0 terr1 terr2

TEST_UNIT "invade" = get_armies state terr1 ===  current1
TEST_UNIT "invade" = get_armies state terr2 ===  current2

let state = set_num_armies state terr1 (3)

let current1 = get_armies state terr1
let current2 = get_armies state terr2
let state = invade state 3 terr1 terr2

TEST_UNIT "invade" = get_armies state terr1 ===  current1 - 3
TEST_UNIT "invade" = get_armies state terr2 ===  current2 + 3

let current1 = get_armies state terr1
let current2 = get_armies state terr2
let state = invade state 2 terr2 terr1

TEST_UNIT "invade" = get_armies state terr1 ===  current1 + 2
TEST_UNIT "invade" = get_armies state terr2 ===  current2 - 2
