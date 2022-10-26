open Player
open Property
open Tile
open Board

let starting_money = 1500

type state = { players : player list }

let init_state (players : player list) : state = { players }
let check_properties purchased location = List.mem location purchased
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
  | Property x -> buy_property player x roll
  | _ -> charge player 0 roll

let rec one_turn (s : state) (player : player) purchased =
  print_endline
    ("---------------------Starting turn for player " ^ Player.name player
   ^ "---------------------");
  let roll = string_of_int (Random.int 5 + 1) in
  let tell_roll = "Your roll is " ^ roll in
  print_endline tell_roll;

  let old_position = location player in
  let new_position = (old_position + int_of_string roll) mod 40 in

  let updated_player = move_to player new_position in

  if Monopoly.player_passed_go old_position new_position then
    print_endline "You have passed Go! You win $200";

  let if_player_passed_go =
    if Monopoly.player_passed_go old_position new_position then
      pay updated_player 200
    else updated_player
  in

  inform_player s if_player_passed_go;
  print_endline
    "What would you like to do? Purchase this property (enter 'P') or do \
     nothing (enter any other key) ";
  print_string "> ";
  match Monopoly.parse_user_input (read_line ()) with
  | "P" ->
      if check_properties purchased new_position then (
        print_endline "ERROR: This property has already been purchased.";
        print_endline ("End of turn for " ^ Player.name player);
        (player, purchased))
      else
        let player =
          purchase_property player
            (List.nth board (location updated_player))
            (int_of_string roll)
        in
        print_endline "Congratulations, you have just bought a property! ";
        print_endline ("End of turn for " ^ Player.name player);
        (player, update_properties purchased new_position)
  | _ ->
      let player = charge player 0 (int_of_string roll) in
      print_endline ("End of turn for " ^ Player.name player);
      (player, purchased)

let rec take_turns purchased (s : state) : state =
  match s.players with
  | [] -> { players = [] }
  | h :: t ->
      {
        players =
          (match one_turn s h purchased with
          | p, s -> p :: (take_turns s { players = t }).players);
      }
