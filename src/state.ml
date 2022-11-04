open Player
open Property
open Tile
open Board
open Chance

let starting_money = 1500

type state = {
  players : player list;
  purchased_properties : int list;
}

let init_state (players : player list) : state =
  { players; purchased_properties = [] }

let check_properties purchased location =
  match List.nth board location with
  | Property x -> List.mem location purchased
  | _ -> false
(*List.mem location purchased *)

let update_properties purchased location = location :: purchased

let inform_player (s : state) (player_info : player) : unit =
  print_endline ("Starting turn for player " ^ Player.name player_info);

  let money = cash player_info in
  let position = location player_info in
  let landed = List.nth board position in

  print_endline ("You currently have $" ^ string_of_int money);
  print_endline ("You are currently at " ^ tileName landed);
  print_endline
    ("This property costs $ " ^ string_of_int (Tile.get_price landed))

let rec init_players players_lst =
  print_endline "Please enter your name: ";
  print_string "> ";

  match read_line () with
  | name -> (
      let new_player = init_player name starting_money in

      let updated_players = players_lst @ [ new_player ] in
      print_endline
        ("Successfully created new player named " ^ Player.name new_player);
      print_endline "Enter another player? Enter 'Yes' or 'No'";

      match String.lowercase_ascii (read_line ()) with
      | "yes" -> init_players updated_players
      | "no" -> updated_players
      | _ ->
          print_endline "I didn't understand that";
          updated_players)

let purchase_property (player : player) property (roll : int) =
  match property with
  | Property x -> 
    print_endline "Congratulations, you have just bought a property";
    buy_property player x
  | IncomeTax | LuxuryTax-> 
    print_endline "Sorry, this is a tax! You cannot purchase 
    this property! Please enter 'T' to pay this tax. ";
    charge player (0)
  | _ -> charge player 0

let pay_tax (player: player) property = 
  match property with 
  |IncomeTax -> charge player 200 
  |LuxuryTax -> charge player 100 
  |_ -> 
    print_endline "You don't have to pay any tax :) !";
    charge player 0

let rec one_turn (s : state) (player : player) =
  print_endline
    ("---------------------Starting turn for player " ^ Player.name player
   ^ "---------------------");
  let roll = string_of_int (Random.int 6 + 1 + (Random.int 6 + 1)) in
  let tell_roll = "Your roll is " ^ roll in
  print_endline tell_roll;

  let old_position = location player in
  let new_position = (old_position + int_of_string roll) mod 40 in

  let updated_player = move_to player new_position in

  let if_player_passed_go =
    if Monopoly.player_passed_go old_position new_position then (
      print_endline "You have passed Go! You win $200";
      pay updated_player 200)
    else updated_player
  in

  inform_player s if_player_passed_go;
  print_endline
    "What would you like to do? Purchase this property (enter 'P') or do \
     nothing (enter any other key) ";
  print_string "> ";
  match Monopoly.parse_user_input (read_line ()) with
  | "P" ->
      if check_properties s.purchased_properties new_position then (
        print_endline "ERROR: This property has already been purchased.";
        print_endline ("End of turn for " ^ Player.name if_player_passed_go);
        (if_player_passed_go, s.purchased_properties))
      else
        let player_purchased =
          purchase_property if_player_passed_go
            (List.nth board (location if_player_passed_go))
            (int_of_string roll)
        in
        print_endline "Congratulations, you have just bought a property! ";
        print_endline ("End of turn for " ^ Player.name player_purchased);
        (player_purchased, update_properties s.purchased_properties new_position)
  |"T" -> 
    let tax = List.nth board (location if_player_passed_go) in 
    let player_payed = pay_tax player tax in
    print_endline ("End of turn for " ^ Player.name player_payed);
    (player_payed, s.purchased_properties)

  | _ ->
      print_endline ("End of turn for " ^ Player.name if_player_passed_go);
      (if_player_passed_go, s.purchased_properties)

let rec take_turns (s : state) : state =
  match s.players with
  | [] -> { players = []; purchased_properties = [] }
  | h :: t -> (
      match one_turn s h with
      | p, new_s ->
          {
            players =
              p
              :: (take_turns { players = t; purchased_properties = new_s })
                   .players;
            purchased_properties = new_s;
          })
