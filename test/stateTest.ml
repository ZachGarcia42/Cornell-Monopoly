open OUnit2
open Game
open State
open Player

(* TODO: For testing our state implementation, we achieved X% coverage using
   glass box and black box OUnit tests, so [no] manual testing was required.
   However, our play testing also inherently tests these functions since they
   are an integral part of Monopoly. *)

let test (name : string) exp act = name >:: fun _ -> assert_equal exp act
let players = [ init_player "Zach" 1 ]

let tests =
  [ test "player list" players (players |> init_state |> player_list) ]
