open OUnit2
open Game
open Chance

(* For testing our chance implementation, we achieved 100% coverage using glass
   box and black box OUnit tests, so no manual testing was required. However,
   our play testing also inherently tests these functions since they are an
   integral part of Monopoly. *)

let chance =
  [init_chance "Chance: Advancement" 200 "Advance to Go! Collect $200!" "Go" 0; 
  init_chance "Get out of Jail Free" 0 "Acquire a Get of Jail Free Card"
  "Current" 0;
  ]


let tests =
  [
    ( "command test" >:: fun _ ->
      assert_equal "Advance to Go! Collect $200!" (command (List.nth chance 0)) );
    ("destination test" >:: fun _ -> assert_equal "Go" (destination (List.nth chance 0)));
    ("destination test" >:: fun _ -> assert_equal "Current" (destination (List.nth chance 1)));
    ("charge test" >:: fun _ -> assert_equal 0 (price (List.nth chance 1)));
    ("charge test" >:: fun _ -> assert_equal 200 (price (List.nth chance 0)));
    ("name test" >:: fun _ -> assert_equal "Chance: Advancement" (name (List.nth chance 0)));
    ("name test" >:: fun _ -> assert_equal "Get out of Jail Free" (name (List.nth chance 1)));
  ]
