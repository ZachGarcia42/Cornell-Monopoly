open OUnit2
open Game
open State
open Player
open Property
open Tile

(* For testing our state implementation, we achieved 6% coverage using glass box
   and black box OUnit tests, so manual testing was required due to the player
   input requirements present in many of the state functions. Our play testing
   also inherently tests these functions since they are an integral part of the
   gameplay. *)
let zach = init_player "Zach" 1500
let boardwalk_t = init_property "Boardwalk" Blue 400 39
let boardwalk = Property boardwalk_t
let player2 = purchase_property zach boardwalk
let players = [ zach; player2 ]
let default_state = init_state players [] 0
let state_2 = init_state players [ 39; 37 ] 0
let go = Go
let it = IncomeTax
let fp = FreeParking
let lt = LuxuryTax
let test (name : string) exp act = name >:: fun _ -> assert_equal exp act

let test_purchase (name : string) exp tile =
  test name exp (purchase_property (List.hd players) tile |> cash)

let tests =
  [
    test "player list" players (init_state players [] 0 |> player_list);
    test_purchase "purchase luxury tax" 1400 lt;
    test_purchase "purchase income tax" 1300 it;
    test_purchase "purchase go" 1500 go;
    test_purchase "purchase boardwalk" 1100 boardwalk;
    test_purchase "purchase free parking" 1500 fp;
    test "no purchased properties" [] (purchased_properties default_state);
    test "add property" [ 39 ] (add_properties [] 39);
    test "money jar empty" 0 (money_jar default_state);
    test "check_properties boardwalk" true (check_properties state_2 39);
    test "check_properties empty list" false (check_properties default_state 39);
    test "check_properties not in list" false (check_properties state_2 3);
    test "check_properties not a property" false (check_properties state_2 2);
    test "remove_properties removes" false
      (check_properties (remove_properties state_2 39) 39);
    test "is property owned" true (is_property_owned boardwalk_t players);
    test "no players can't own" false (is_property_owned boardwalk_t []);
    test "find owner of boardwalk" "Zach" (find_owner boardwalk_t players);
    test "no one is owner" "No one" (find_owner boardwalk_t []);
  ]
