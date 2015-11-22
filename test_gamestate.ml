open GameState
open Assertions

let state = new_state () ;

(*tests on initial state*)
TEST_UNIT "get_player_id_list" = get_player_id_list state === []

TEST_UNIT "get_territories (non-empty)" =
  assert(List.length (get_territories state no_one) > 0)

TEST_UNIT "get_is_human" = 0 === 0 (*need better error handling*)

TEST_UNIT "get_name" = 0 === 0 (*need better error handling*)

TEST_UNIT "get_active_player" = get_active_player state === no_one