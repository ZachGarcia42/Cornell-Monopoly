open OUnit2
open Game
open Tile
open Property
open Chance
open Chest
open Board

(* For testing our tile implementation, we achieved 100% coverage using glass
   box and black box OUnit tests, so no manual testing was required. However,
   our play testing also inherently tests these functions since they are an
   integral part of Monopoly. *)

let boardwalk = Property (init_property "Boardwalk" Blue 400 39)
let go = Go

let cc =
  CommunityChest (init_chest "Advance to Go! Collect $200" "Go" 200 "Bank")

let it = IncomeTax

let chance =
  Chance
    (init_chance "Chance: Advancement" 200 "Advance to Go! Collect $200!" "Go" 0)

let chest = CommunityChest (init_chest "Chest" "Go" 200 "Bank")
let jv = JustVisiting
let fp = FreeParking
let jail = GoToJail
let lt = LuxuryTax

let name_test (name : string) expected_output input =
  name >:: fun _ -> assert_equal expected_output (tileName input)

let price_test (name : string) exp input =
  name >:: fun _ -> assert_equal exp (get_price input)

let pos_test (name : string) exp input =
  name >:: fun _ -> assert_equal exp (get_pos board input 0)

let tests =
  [
    name_test "property name" "Boardwalk" boardwalk;
    name_test "go name" "Go" go;
    name_test "community chest name" "Community Chest" cc;
    name_test "IncomeTax name" "Income Tax" it;
    name_test "chance name" "Chance" chance;
    name_test "Just Visiting name" "Just Visiting" jv;
    name_test "Free Parking name" "Free Parking" fp;
    name_test "Go to Jail name" "Go To Jail" jail;
    name_test "Luxury Tax Name" "Luxury Tax" lt;
    price_test "property price" 400 boardwalk;
    price_test "income tax price" ~-200 it;
    price_test "luxury tax price" ~-100 lt;
    price_test "chance price" 200 chance;
    price_test "other price" 0 jv;
    price_test "chest price" 200 chest;
    pos_test "chance" 7 "Chance";
    pos_test "just visiting" 10 "Just Visiting";
    ("empty board" >:: fun _ -> assert_equal 0 (get_pos [] "Just Visiting" 0));
  ]
