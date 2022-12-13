open OUnit2
open Game
open Chest

let cards =
  [
    init_chest "Advance to Go! Collect $200" "Go" 200 "Bank";
    init_chest "Doctor's Fees! Pay $50" "Current" ~-50 "Bank";
  ]

let test name exp act = name >:: fun _ -> assert_equal exp act

let tests =
  [
    test "command test" "Advance to Go! Collect $200" (name (List.nth cards 0));
    test "destination test" "Go" (destination (List.nth cards 0));
    test "destination test" "Current" (destination (List.nth cards 1));
    test "payment test" ~-50 (payment (List.nth cards 1));
    test "payment test" 200 (payment (List.nth cards 0));
    test "payment source test" "Bank" (payment_source (List.nth cards 0));
    test "name test" "Doctor's Fees! Pay $50" (name (List.nth cards 1));
  ]
