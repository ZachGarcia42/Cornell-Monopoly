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
let players = [ init_player "Zach" 1500 ]
let default_state = init_state players [] 0
let boardwalk = Property (init_property "Boardwalk" Blue 400 39)
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
  ]
