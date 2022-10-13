open OUnit2
open Game
open Monopoly
open Property
open Player

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
    property_test "Boardwalk name" "Boardwalk" (Property.name boardwalk);
    property_test "Boardwalk color" Blue (color boardwalk);
    property_test "Boardwalk price" 400 (price boardwalk);
  ]

let player_test (name : string) expected_output input : test =
  name >:: fun _ -> assert_equal expected_output input

let zach = init_player "Zach" 1 1500

let player_tests =
  [
    player_test "initialize name" "Zach" (Player.name zach);
    player_test "initialize pos" 1 (position zach);
    player_test "initialize cards" 0 (cards zach);
    player_test "initialize tile" 0 (location zach);
    player_test "initialize properties" [] (properties zach);
    player_test "initialize cash" 1500 (cash zach);
    player_test "initialize in_jail" false (in_jail zach);
    player_test "go to jail" true (zach |> go_to_jail |> in_jail);
    player_test "move to 10" 10 (move_to zach 10 |> location);
    player_test "pay 50" 1550 (pay zach 50 |> cash);
    player_test "charge 50" 1450 (charge zach 50 |> cash);
    player_test "has boardwalk" true
      (has_property (buy_property zach boardwalk) boardwalk);
    player_test "paid for boardwalk" 1100 (buy_property zach boardwalk |> cash);
  ]

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
