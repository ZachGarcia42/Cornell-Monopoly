open OUnit2
open Game
open Chance

(* For testing our chance implementation, we achieved 100% coverage using glass
   box and black box OUnit tests, so no manual testing was required. However,
   our play testing also inherently tests these functions since they are an
   integral part of Monopoly. *)

let chance =
  [
    init_chance "Chance: Advancement" 200 "Advance to Go! Collect $200!" "Go" 0;
    init_chance "Get out of Jail Free" 0 "Acquire a Get of Jail Free Card"
      "Current" 0;
    init_chance "Move back 3 spaces" 0 "Move back 3 spaces." "3" ~-3;
  ]

let test name exp act = name >:: fun _ -> assert_equal exp act

let tests =
  [
    test "command test" "Advance to Go! Collect $200!"
      (command (List.nth chance 0));
    test "destination test" "Go" (destination (List.nth chance 0));
    test "destination test" "Current" (destination (List.nth chance 1));
    test "charge test" 0 (price (List.nth chance 1));
    test "charge test" 200 (price (List.nth chance 0));
    test "name test" "Chance: Advancement" (name (List.nth chance 0));
    test "name test" "Get out of Jail Free" (name (List.nth chance 1));
    test "translation test" 0 (rel_space_translation (List.nth chance 1));
    test "non-zero translation test" ~-3
      (rel_space_translation (List.nth chance 2));
  ]
