open OUnit2
open Game
open Monopoly
open Property

let test_is_player_turn (name : string) (expected_output : bool) board =
  name >:: fun _ -> assert_equal expected_output (is_player_turn board)

let monopoly_tests = []
let board_tests = []

(* Property tests *)
let property_test (name : string) expected_output input : test =
  name >:: fun _ -> assert_equal expected_output input

let boardwalk = init_property "Boardwalk" Blue 400

let property_tests =
  [
    property_test "Boardwalk name" "Boardwalk" (name boardwalk);
    property_test "Boardwalk color" Blue (color boardwalk);
    property_test "Boardwalk price" 400 (price boardwalk);
  ]

let player_tests = []
let chance_tests = []

let test_suite =
  "Monopoly test suite"
  >::: List.flatten
         [
           monopoly_tests;
           board_tests;
           property_tests;
           player_tests;
           chance_tests;
         ]

let _ = run_test_tt_main test_suite
