open OUnit2
open Game
open Player
open Property

(* For testing our player implementation, we achieved 100% coverage using OUnit
   tests, so no manual testing was required. However, our play testing also
   inherently tests players since they are an integral part of Monopoly. *)

let player_test (name : string) expected_output input : test =
  name >:: fun _ -> assert_equal expected_output input

let zach = init_player "Zach" 1500
let boardwalk = init_property "Boardwalk" Blue 400 39

let tests =
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
    player_test "charge 50" 1450 (charge zach 50 |> cash);
    player_test "has boardwalk" true
      (has_property (buy_property zach boardwalk) boardwalk);
    player_test "paid for boardwalk" 1100 (buy_property zach boardwalk |> cash);
    player_test "net worth" 1500 (buy_property zach boardwalk |> net_worth);
    player_test "not worth less" 1450
      (charge (buy_property zach boardwalk) 50 |> net_worth);
  ]
