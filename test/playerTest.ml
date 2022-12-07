open OUnit2
open Game
open Chance
open Chest
open Player
open Property

(* For testing our player implementation, we achieved 100% coverage using OUnit
   tests, so no manual testing was required. However, our play testing also
   inherently tests players since they are an integral part of Monopoly. *)

let player_test (name : string) expected_output input : test =
  name >:: fun _ -> assert_equal expected_output input

let int_test (name : string) expected_output input : test =
  name >:: fun _ -> assert_equal expected_output input ~printer:string_of_int

let zach = init_player "Zach" 1500
let players = [ zach ]
let boardwalk = init_property "Boardwalk" Blue 400 39
let park_place = init_property "Park Place" Blue 380 37

let chance_go =
  init_chance "Chance: Advancement" 200 "Advance to Go! Collect $200!" "Go" 0

let chance_adv =
  init_chance "Chance: Advancement" 0 "Go to Just Visiting" "Just Visiting" 0

let chance_blank = init_chance "Chance" 0 "" "Current" 0
let chance_mm = init_chance "Chance: Money Made" 200 "" "Current" 0
let chance_pay = init_chance "Chance: Payment Required" 100 "" "Current" 0
let chance_move = init_chance "Chance: Move Backwards" 0 "" "Current" ~-3

let chance_goojf =
  init_chance "Chance: Get out of Jail Free" 0 "Acquire a Get of Jail Free Card"
    "Current" 0

let cc_go = init_chest "Advance to Go! Collect $200" "Go" 200 "Bank"
let cc_doc = init_chest "Doctor's Fees! Pay $50" "Current" ~-50 "Bank"

let cc_bday =
  init_chest "It is your birthday! Collect $10 from every player" "Current" 10
    "All Players"

let cc_move = init_chest "" "Income Tax" 0 "Bank"
let cc_goojf = init_chest "Get out of Jail Free Card earned" "Current" 0 "Bank"

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
    player_test "turns in jail" 0 (turns_in_jail zach);
    player_test "1 turn in jail" 1 (zach |> jail_turn |> turns_in_jail);
    player_test "not in jail after leaving" false
      (zach |> go_to_jail |> leave_jail |> in_jail);
    player_test "no turns in jail after leaving" 0
      (zach |> go_to_jail |> jail_turn |> jail_turn |> leave_jail
     |> turns_in_jail);
    player_test "2 turns in jail" 2
      (zach |> go_to_jail |> jail_turn |> jail_turn |> turns_in_jail);
    player_test "add a card" 1 (zach |> add_get_out_card |> cards);
    player_test "add then remove card" 0
      (zach |> add_get_out_card |> use_card |> cards);
    player_test "use a card" false
      (zach |> add_get_out_card |> go_to_jail |> use_card |> in_jail);
    player_test "sell property cash" 1300
      (sell_property (buy_property zach boardwalk) boardwalk |> cash);
    player_test "sell property has prop" false
      (has_property
         (sell_property (buy_property zach boardwalk) boardwalk)
         boardwalk);
    player_test "not chance does nothing" zach
      (unlock_chance_card zach (Property boardwalk));
    player_test "chance pays money" 1700
      (cash (handle_chance zach chance_go (Chance chance_go)));
    player_test "chance does nothing" zach
      (handle_chance zach chance_blank (Chance chance_blank));
    player_test "chance adv does not pass go" 1500
      (cash (handle_chance zach chance_adv (Chance chance_adv)));
    player_test "chance adv does nothing the second time" 1500
      (handle_chance
         (handle_chance zach chance_adv (Chance chance_adv))
         chance_adv (Chance chance_adv)
      |> cash);
    player_test "chance goojf adds a card" 1
      (cards (handle_chance zach chance_goojf (Chance chance_goojf)));
    player_test "chance mm adds 200" 1700
      (cash (handle_chance zach chance_mm (Chance chance_mm)));
    player_test "chance pay takes 100" 1400
      (cash (handle_chance zach chance_pay (Chance chance_pay)));
    int_test "chance move moves" 4
      (location (handle_chance zach chance_move (Chance chance_move)));
    player_test "not cc does nothing" players
      (unlock_comm_chest_card zach (Property boardwalk) players);
    player_test "cc move does nothing"
      (handle_cc zach players cc_move (CommunityChest cc_move))
      (handle_cc zach
         (handle_cc zach players cc_move (CommunityChest cc_move))
         cc_move (CommunityChest cc_move));
    player_test "cc go pays 200" 1700
      (cash (List.nth (handle_cc zach players cc_go (CommunityChest cc_go)) 0));
    player_test "cc doc charges 50" 1450
      (cash
         (List.nth (handle_cc zach players cc_doc (CommunityChest cc_doc)) 0));
    player_test "cc bday pays 0" 1500
      (cash
         (List.nth (handle_cc zach players cc_bday (CommunityChest cc_bday)) 0));
    player_test "cc goojf adds a card" 1
      (cards
         (List.nth
            (handle_cc zach players cc_goojf (CommunityChest cc_goojf))
            0));
    player_test "pay income tax" 1300 (cash (pay_tax zach IncomeTax));
    player_test "pay luxury tax" 1400 (cash (pay_tax zach LuxuryTax));
    player_test "pay no tax" 1500 (cash (pay_tax zach (Property boardwalk)));
    player_test "list no properties" "" (string_list_properties zach);
    player_test "list one properties" " | Boardwalk"
      (string_list_properties (buy_property zach boardwalk));
    player_test "list two properties" " | Boardwalk | Park Place"
      (string_list_properties
         (buy_property (buy_property zach park_place) boardwalk));
    player_test "none prop" None (player_name_to_property zach "Boardwalk");
    player_test "boardwalk prop" (Some boardwalk)
      (player_name_to_property (buy_property zach boardwalk) "BOARDWALK");
    player_test "boardwalk prop" (Some boardwalk)
      (player_name_to_property
         (buy_property (buy_property zach boardwalk) park_place)
         "BOARDWALK");
  ]
