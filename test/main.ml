open OUnit2
open Game
open Monopoly
open Property
open Player
open Board

let board = Board.board
let identity s = s

let square_landed_test (name : string) expected_output board init_pos dice_roll
    : test =
  name >:: fun _ ->
  assert_equal expected_output
    (square_landed board init_pos dice_roll)
    ~printer:identity

let parse_user_input_test (name : string) (expected_output : string)
    (input : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (parse_user_input input) ~printer:identity

let player_passed_go_test (name : string) (expected_output : bool) (input : int)
    (input2 : int) : test =
  name >:: fun _ -> assert_equal expected_output (player_passed_go input input2)

let monopoly_tests =
  [
    square_landed_test "testing square landed" "Baltic Avenue" board 1 2;
    square_landed_test "testing square landed" "Vermont Avenue" board
      (List.length board - 1)
      9;
    square_landed_test "testing square landed" "Community Chest" board
      (List.length board - 1)
      3;
    square_landed_test "testing square landed" "Luxury Tax" board
      (List.length board - 3)
      1;
    parse_user_input_test "testing parse user input" "P" "   P";
    parse_user_input_test "testing parse user input" "P" "    p         ";
    parse_user_input_test "testing parse user input" "" "      ";
    player_passed_go_test "testing player passed go" true 3 2;
    player_passed_go_test "testing player passed go" false 4 7;
    player_passed_go_test "testing player passed go" true 38 0;
  ]

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

let zach = init_player "Zach" 1500

let player_tests =
  [
    player_test "initialize name" "Zach" (Player.name zach);
    player_test "initialize cards" 0 (cards zach);
    player_test "initialize tile" 0 (location zach);
    player_test "initialize properties" [] (properties zach);
    player_test "initialize cash" 1500 (cash zach);
    player_test "initialize in_jail" false (in_jail zach);
    player_test "go to jail" true (zach |> go_to_jail |> in_jail);
    player_test "move to 10" 10 (move_to zach 10 |> location);
    player_test "pay 50" 1550 (pay zach 50 |> cash);
    player_test "charge 50" 1450 (charge zach 50 2 |> cash);
    player_test "has boardwalk" true
      (has_property (buy_property zach boardwalk 2) boardwalk);
    player_test "paid for boardwalk" 1100 (buy_property zach boardwalk 2 |> cash);
    
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
