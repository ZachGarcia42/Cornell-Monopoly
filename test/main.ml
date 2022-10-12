open OUnit2

open Game.Monopoly

let test_is_player_turn(name: string)(expected_output: bool)(board) = 
  name >:: fun _ ->
  assert_equal expected_output (is_player_turn board)

let monopoly_tests = []

let suite = 
  "test suite for Cornellopoly"
  >::: List.flatten [monopoly_tests]

let _ = run_test_tt_main suite




