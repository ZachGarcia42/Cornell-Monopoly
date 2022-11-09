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

let player_list state = state.players

let init_state (players : player list) : state =
  { players; purchased_properties = [] }

(** [check_properties purchased location] returns true iff the property at index
    [location] of the game board has been purchased. False otherwise. *)
let check_properties purchased location =
  match List.nth board location with
  | Property x -> List.mem location purchased
  | _ -> false
(*List.mem location purchased *)

(** [update_properties purchased location] adds the property at index [location]
    to the list of purchased properties. *)
let update_properties purchased location = location :: purchased

(** [inform_player s player_info] logs important information for a player
    [player_info] based on data stored in state [s]. *)
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
  | Go ->
      print_endline "Collect $200 here! You can't purchase this";
      charge player 0
  | Property x ->
      print_endline "Congratulations, you have just bought a property";
      buy_property player x
  | IncomeTax | LuxuryTax ->
      print_endline
        "Sorry, this is a tax! You cannot purchase \n\
        \    this property! Deducting 200 to pay the tax! ";
      charge player 200
  | _ -> charge player 0

let pay_tax (player : player) property =
  match property with
  | IncomeTax -> charge player 200
  | LuxuryTax -> charge player 100
  | _ ->
      print_endline "You don't have to pay any tax :) !";
      charge player 0

let unlock_destination (dest : string) property =
  let rec helper (dest : string) =
    match board with
    | [] -> None
    | h :: t -> if tileName h = dest then Some h else helper dest
  in
  match helper dest with
  | None -> property
  | Some tile -> tile

let rec get_index_of_dest dest acc =
  match board with
  | [] -> 0
  | h :: t ->
      if List.nth board acc = dest then acc else get_index_of_dest dest (acc + 1)

let unlock_chance_card (player : player) property =
  match property with
  | Chance c ->
      (* TODO: Pattern match against destinations to see where the player should
         be moved to, if at all*)
      let dest = Chance.destination c in
      if dest = "Current" then player
      else
        let new_dest = unlock_destination dest property in
        let idx = get_index_of_dest new_dest 0 in
        Player.move_to player idx
      (*player *)
  | _ ->
      print_endline "This is not a Chance Card!";
      charge player 0

(** [check_player_properties prop properties] is true iff one of the properties
    matches [prop]. *)
let rec check_player_properties prop = function
  | [] -> false
  | property :: t -> property = prop || check_player_properties prop t

(** [is_property_owned property] is true iff [property] is owned by one of the
    players. *)
let rec is_property_owned (property : Property.t) players =
  match players with
  | [] -> false
  | player :: t ->
      check_player_properties property (properties player)
      || is_property_owned property t

let charge_inform (p : Property.t) (player : Player.player) =
  print_endline ("You are being charged " ^ string_of_int (Property.price p))

(** [charged_player player property] is the updated player after they've been
    charged for landing on [property]*)
let charged_player (player : Player.player) (property : Property.t) =
  Player.charge player (Property.price property)

let rec one_turn (s : state) (player : player) =
  print_endline
    ("---------------------Starting turn for player " ^ Player.name player
   ^ "---------------------");
  let roll = string_of_int (Random.int 6 + 1 + (Random.int 6 + 1)) in
  let tell_roll = "Your roll is " ^ roll in
  print_endline tell_roll;

  let old_position = location player in
  let new_position =
    (old_position + int_of_string roll) mod List.length Board.board
  in

  let updated_player = move_to player new_position in

  let if_player_passed_go =
    if Monopoly.player_passed_go old_position new_position then (
      print_endline "You have passed Go! You win $200";
      pay updated_player 200)
    else updated_player
  in

  let updated_player = if_player_passed_go in

  inform_player s updated_player;

  (* TODO: This is a temporary function to charge players for landing on a
     property as needed. Future refactoring will be needed for a more organized
     way of charging the player in general for all reasons and updating the
     player *)
  let updated_player =
    match List.nth Board.board new_position with
    | Property p ->
        if is_property_owned p s.players then charged_player updated_player p
        else updated_player
    | _ -> updated_player
  in

  print_endline "Prompting next action...";
  let prompt_next_action =
    match List.nth Board.board new_position with
    | Go -> print_endline "You are on the Go tile"
    | Property p ->
        if is_property_owned p s.players then charge_inform p player
        else
          print_endline
            ("This property costs $ "
            ^ string_of_int (Property.price p)
            ^ "Attempt to purchase this property? Enter 'P' if you wish to do \
               so")
    | CommunityChest ->
        print_endline
          "You can draw a Community Chest Card. Press 'C' to proceed"
    | IncomeTax ->
        print_endline "You need to pay your taxes! Enter 'T' to continue."
    | Chance _ ->
        print_endline
          "You have landed on a Chance Square! Enter 'C' to proceed."
    | JustVisiting ->
        print_endline
          "You are just visiting your old Dyson pal (who recently committed \n\
           financial fraud) in jail. No action needs to be taken – enter any \
           other key to continue. "
    | FreeParking ->
        print_endline
          "You have landed on free parking! Enter 'Collect' to collect your \
           rewards!"
    | _ ->
        print_endline
          "Enter 'Q' to quit the game, or do nothing (enter any other key)."
  in
  prompt_next_action;
  match Monopoly.parse_user_input (read_line ()) with
  | "P" ->
      if check_properties s.purchased_properties new_position then (
        print_endline "ERROR: This property has already been purchased.";
        print_endline ("End of turn for " ^ Player.name updated_player);
        (updated_player, s.purchased_properties))
      else
        let property_price =
          match List.nth board (location updated_player) with
          | Property p -> Property.price p
          | _ -> 0
        in
        if Player.cash updated_player < property_price then (
          print_endline
            "Sorry, you do not have enough money to purchase this property!";
          (updated_player, s.purchased_properties))
        else
          let player_purchased =
            purchase_property updated_player
              (List.nth board (location updated_player))
              (int_of_string roll)
          in
          print_endline
            "Congratulations! You have successfully purchased this property.";
          print_endline ("End of turn for " ^ Player.name player_purchased);
          ( player_purchased,
            update_properties s.purchased_properties new_position )
  | "C" ->
      print_endline "Drawing a chance card...";
      (* TODO: Implement drawing a chance card. *)
      (updated_player, s.purchased_properties)
  | "T" ->
      let tax = List.nth board (location updated_player) in
      let player_payed = pay_tax player tax in
      if Player.cash player < 0 then
        print_endline
          ("SORRY, " ^ Player.name player_payed ^ " has gone bankrupt!")
      else print_endline ("End of turn for " ^ Player.name player_payed);
      (player_payed, s.purchased_properties)
  | "Q" ->
      print_endline "Thank you for playing Cornellopoly! We hope you had fun!";
      exit 0
  | _ ->
      print_endline ("End of turn for " ^ Player.name updated_player);
      (updated_player, s.purchased_properties)

let rec take_turns (s : state) : state =
  match s.players with
  | [] -> { players = []; purchased_properties = [] }
  | h :: t -> (
      match one_turn s h with
      | p, new_s ->
          if Player.cash p < 0 then
            {
              players =
                (take_turns { players = t; purchased_properties = new_s })
                  .players;
              purchased_properties = new_s;
            }
          else
            {
              players =
                p
                :: (take_turns { players = t; purchased_properties = new_s })
                     .players;
              purchased_properties = new_s;
            })
