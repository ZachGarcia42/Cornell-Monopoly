open OUnit2
open Game
open Property

(* For testing our property implementation, we achieved 100% coverage using
   OUnit tests, so no manual testing was required. However, our play testing
   also inherently tests properties since they are an integral part of
   Monopoly. *)

let property_test (name : string) expected_output input : test =
  name >:: fun _ -> assert_equal expected_output input

let boardwalk = init_property "Boardwalk" Blue 400 39

let tests =
  [
    property_test "Boardwalk name" "Boardwalk" (Property.name boardwalk);
    property_test "Boardwalk color" Blue (color boardwalk);
    property_test "Boardwalk price" 400 (price boardwalk);
  ]
