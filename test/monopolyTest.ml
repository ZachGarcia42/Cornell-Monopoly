open OUnit2
open Game
open Monopoly
open Tile

(* For testing our monopoly file implementation, we achieved 100% coverage using
   OUnit tests, so no manual testing was required. However, our play testing
   also inherently tests these functions since they are an integral part of
   Monopoly. *)

let board = Board.board
let identity s = s
let gotojailpos = get_pos board (tileName GoToJail) 0
let jailpos = get_pos board (tileName JustVisiting) 0

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

let tests =
  [
    square_landed_test "testing square landed" "Mary Donlon Hall" board 1 2;
    square_landed_test "testing square landed" "Clara Dickson Hall" board
      (List.length board - 1)
      9;
    square_landed_test "testing square landed" "Community Chest" board
      (List.length board - 1)
      3;
    square_landed_test "testing square landed" "Luxury Tax" board
      (List.length board - 3)
      1;
    square_landed_test "testing negative result" "Upson Hall" board 0 ~-3;
    parse_user_input_test "testing parse user input" "P" "   P";
    parse_user_input_test "testing parse user input" "P" "    p         ";
    parse_user_input_test "testing parse user input" "" "      ";
    player_passed_go_test "testing player passed go" true 3 2;
    player_passed_go_test "testing player passed go" false gotojailpos jailpos;
    player_passed_go_test "testing player passed go" false 4 7;
    player_passed_go_test "testing player passed go" true 38 0;
  ]
