open GameState
open Assertions
open BeginTurn

let state = new_state ()
let all_territories = get_all_territories state

TEST_UNIT "award_pieces" = award_pieces state no_one ===
  (List.length all_territories)/3

let p1 = create_player 1
let state = add_player state p1 "test player1" true

TEST_UNIT "award_pieces" = award_pieces state p1 === 1

let terr1 = List.hd (get_territories state no_one)
let state = set_territory_owner state terr1 p1

TEST_UNIT "award_pieces" = award_pieces state p1 === 1

let current = get_armies state terr1
let state = place_piece state 0 p1 terr1

TEST_UNIT "place_piece" = get_armies state terr1 === current

let state = place_piece state 2 p1 terr1

TEST_UNIT "place_piece" = get_armies state terr1 === current + 2

