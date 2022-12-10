open OUnit2
open Game
open Property

(* For testing our property implementation, we achieved 100% coverage using
   OUnit tests, so no manual testing was required. However, our play testing
   also inherently tests properties since they are an integral part of Monopoly.
   The combination of these approaches demonstrates the correctness of this
   module.*)

let property_test (name : string) expected_output input : test =
  name >:: fun _ -> assert_equal expected_output input

let boardwalk = init_property "Boardwalk" Blue 400 39

let tests =
  [
    property_test "Boardwalk name" "Boardwalk" (Property.name boardwalk);
    property_test "Boardwalk color" Blue (color boardwalk);
    property_test "Boardwalk price" 400 (price boardwalk);
    property_test "string of red" "Red" (string_of_set Red);
    property_test "string of yellow" "Yellow" (string_of_set Yellow);
    property_test "string of green" "Green" (string_of_set Green);
    property_test "string of blue" "Blue" (string_of_set Blue);
    property_test "string of brown" "Brown" (string_of_set Brown);
    property_test "string of light blue" "Light Blue" (string_of_set LightBlue);
    property_test "string of magenta" "Magenta" (string_of_set Magenta);
    property_test "string of orange" "Orange" (string_of_set Orange);
    property_test "string of railroad" "Railroad" (string_of_set Railroad);
    property_test "string of utility" "Utility" (string_of_set Utility);
    ( "set of non-legal string" >:: fun _ ->
      assert_raises (Failure "Illegal property set") (fun () ->
          set_of_string "Hi") );
    property_test "index of boardwalk" 39 (index boardwalk);
  ]
