open OUnit2
open Game
open Chance

(* For testing our chance implementation, we achieved 100% coverage using glass
   box and black box OUnit tests, so no manual testing was required. However,
   our play testing also inherently tests these functions since they are an
   integral part of Monopoly. *)

let chance =
  init_chance "Chance: Advancement" 200 "Advance to Go! Collect $200!" "Go"

let tests =
  [
    ( "command test" >:: fun _ ->
      assert_equal "Advance to Go! Collect $200!" (command chance) );
    ("destination test" >:: fun _ -> assert_equal "Go" (destination chance));
  ]
