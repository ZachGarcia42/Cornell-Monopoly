open Player
open Tile
open Board
open Chance
open Property
open Chest

let starting_money = 1500

type state = {
  players : player list;
  purchased_properties : int list;
  money_jar : int;
}

let player_list state = state.players

let init_state (players : player list) : state =
  { players; purchased_properties = []; money_jar = 0 }

(** [check_properties state location] returns true iff the property at index
    [location] of the game board has been purchased. False otherwise. *)
let check_properties state location =
  match List.nth board location with
  | Property x -> List.mem location state.purchased_properties
  | _ -> false
(*List.mem location purchased *)

(** [add_properties purchased location] adds the property at index [location] to
    the list of purchased properties. *)
let add_properties purchased location = location :: purchased

(**[remove_properties purchased property] removes the property [property] from
   the list of purchased properties*)
let remove_properties purchased property =
  List.filter (fun p -> if p = property then false else true) purchased

(**[string_list_properties players] converts the list of player properties into
   a string*)
let string_list_properties player =
  List.fold_left
    (fun (acc : string) (h : Property.t) -> acc ^ " | " ^ Property.name h)
    "" (properties player)

(** [inform_player s player_info current] tells [player] important info at the
    beginning of their turn, including what they rolled, how much money they
    have, their new position, and if they're on a property, how much that
    property costs. *)
let inform_player (s : state) (player : player) (current_tile : Tile.tile)
    (roll : int) : unit =
  print_endline ("You rolled a " ^ string_of_int roll ^ "!");

  let money = cash player in

  print_endline ("Bank Account balance: $" ^ string_of_int money);
  print_endline ("New position: " ^ tileName current_tile);

  match current_tile with
  | Property p ->
      print_endline
        ("This property is valued at $ "
        ^ string_of_int (Tile.get_price current_tile))
  | _ -> ()

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
      | "yes" | "y" -> init_players updated_players
      | "no" | "n" -> updated_players
      | _ ->
          print_endline "I didn't understand that";
          updated_players)

(*[player_name player str] matches a string [str] to a property that player
  [player] has purchased. Returns None if [str] is not the name of a player's
  property.*)
let rec player_name_to_property (player : player) str =
  match properties player with
  | [] -> None
  | h :: t ->
      if Monopoly.parse_user_input (Property.name h) = str then Some h
      else player_name_to_property (sell_property player h) str

(*[state_sell_prop player property] returns the new type player after the player
  [player] has sold the property [property]*)
let state_sell_prop (player : player) property = sell_property player property

(** [purchase_property player property] is the updated [player] after they have
    purchased [property]. Requires: [property] is a tile, but all uses of this
    function will have it be a tile property. Other match cases are for code
    safety. *)
let purchase_property (player : player) (property : Tile.tile) =
  match property with
  | Go ->
      print_endline "Collect $200 here! You can't purchase this";
      charge player 0
  | Property x ->
      print_endline
        "Congratulations! You have successfully purchased this property.";
      buy_property player x
  | IncomeTax ->
      print_endline
        "Sorry, this is a tax! You cannot purchase \n\
        \    this property! Deducting 200 to pay the tax! ";
      charge player 200
  | LuxuryTax ->
      print_endline
        "Sorry, this is a tax! You cannot purchase \n\
        \    this property! Deducting 100 to pay the tax! ";
      charge player 100
  | _ -> charge player 0

let pay_tax (player : player) property =
  match property with
  | IncomeTax -> charge player 200
  | LuxuryTax -> charge player 100
  | _ ->
      print_endline "You don't have to pay any tax :) !";
      charge player 0

let rec get_pos board dest acc =
  match board with
  | [] -> acc
  | h :: t ->
      let sq = tileName h in
      if sq = dest then acc else get_pos t dest (acc + 1)

let unlock_chance_card (player : player) property =
  match property with
  | Chance c ->
      let dest = Chance.destination c in
      let ctype = Chance.name c in
      let price = Chance.price c in
      if ctype = "Chance: Advancement" then (
        let new_pos = get_pos board dest 0 in
        print_endline ("You have advanced to " ^ dest);
        Player.move_to player new_pos)
      else if ctype = "Chance: Money Made" then pay player price
      else if ctype = "Chance: Payment Required" then charge player price
      else Player.add_get_out_card player
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

(** [find_owner p players] is the name of the owner of property [p] from the
    [players] list *)
let rec find_owner (p : Property.t) (players : Player.player list) =
  match players with
  | [] -> "No one"
  | h :: t -> if Player.has_property h p then Player.name h else find_owner p t

(** [rent_charge_inform s p player] informs the current player whose turn it of
    the owner of the property [p] they landed on and how much they are being
    charged as rent *)
let rent_charge_inform (s : state) (p : Property.t) =
  let owner = find_owner p s.players in
  print_endline ("This property is owned by " ^ owner);
  print_endline
    ("You are being charged "
    ^ string_of_int (Property.price p)
    ^ "for the privilege of staying on their properties. You may not take any \
       other actions, press any key to continue.")

(** [charged_player player property] is the updated player after they've been
    charged for landing on [property]*)
let charged_player (player : Player.player) (property : Property.t) =
  Player.charge player (Property.price property)

let rec one_turn (s : state) (player : player) =
  print_endline
    ("---------------------Starting turn for player " ^ Player.name player
   ^ "---------------------");
  let roll = Random.int 6 + 1 + (Random.int 6 + 1) in

  let old_position = location player in
  let new_position = (old_position + roll) mod List.length Board.board in

  let updated_player = move_to player new_position in

  print_endline
    ("You are starting this turn on "
    ^ Tile.tileName (List.nth Board.board old_position));

  let updated_player =
    if Monopoly.player_passed_go old_position new_position then (
      print_endline "You have passed Go! You win $200";
      pay updated_player 200)
    else updated_player
  in

  let current_tile = List.nth Board.board new_position in

  inform_player s player current_tile roll;

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
  let prompt_next_action state tile player =
    match List.nth Board.board new_position with
    | Go -> print_endline "You are on the Go tile"
    | Property p ->
        if is_property_owned p s.players then rent_charge_inform state p
        else
          print_endline
            ("This property costs $ "
            ^ string_of_int (Property.price p)
            ^ "Attempt to purchase this property? Enter 'P' if you wish to do \
               so")
    | CommunityChest ->
        print_endline
          "You can draw a Community Chest Card. Press 'H' to proceed"
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
  print_endline "Enter 'S' to sell a property";

  ignore prompt_next_action;

  match Monopoly.parse_user_input (read_line ()) with
  | "P" ->
      if check_properties s new_position then (
        print_endline "ERROR: This property has already been purchased.";
        print_endline ("End of turn for " ^ Player.name updated_player);
        (updated_player, s.purchased_properties, s.money_jar))
      else
        let property_price =
          match List.nth board (location updated_player) with
          | Property p -> Property.price p
          | _ -> 0
        in
        if Player.cash updated_player < property_price then (
          print_endline
            "Sorry, you do not have enough money to purchase this property!";
          (updated_player, s.purchased_properties, s.money_jar))
        else
          let player_purchased =
            purchase_property updated_player current_tile
          in

          print_endline ("End of turn for " ^ Player.name player_purchased);
          ( player_purchased,
            add_properties s.purchased_properties (location updated_player),
            s.money_jar )
  | "C" ->
      print_endline "Drawing a chance card...";

      (* TODO: Implement drawing a chance card. *)
      let next_update =
        unlock_chance_card updated_player (List.nth Board.board new_position)
      in
      (next_update, s.purchased_properties, s.money_jar)
  | "H" ->
      print_endline "Drawing a community chest card ...";
      (updated_player, s.purchased_properties, s.money_jar)
  | "T" ->
      print_endline "TAXINGG";
      let taxable_tile = List.nth board (location updated_player) in
      let tax_amt = Tile.get_price (List.nth board (location updated_player)) in
      let player_paid = pay_tax player taxable_tile in
      if Player.cash player < 0 then
        print_endline
          (Player.name player_paid
         ^ " has gone bankrupt, Cornell's overwhelming costs have proved to be \
            too much for them!")
      else print_endline ("End of turn for " ^ Player.name player_paid);
      let updated_player_position =
        Player.move_to player_paid (location updated_player)
      in
      (updated_player_position, s.purchased_properties, s.money_jar + tax_amt)
  | "Q" ->
      print_endline "Thank you for playing Cornellopoly! We hope you had fun!";
      exit 0
  | "Collect" ->
      print_endline
        ("Congrats, You have reaped the rewards of landing on free parking!"
       ^ string_of_int s.money_jar ^ "will be added to your bank account.");
      (updated_player, s.purchased_properties, 0)
  | "S" ->
      print_endline
        "Pick from the following properties, or enter any other value to exit:";
      print_endline (string_list_properties player);
      let inp = Monopoly.parse_user_input (read_line ()) in
      let propholder = player_name_to_property updated_player inp in
      let playernow =
        if
          propholder != None
          && has_property updated_player (Option.get propholder)
        then begin
          print_endline
            (inp ^ " sold t. = End of turn for " ^ Player.name updated_player);
          ignore
            (remove_properties s.purchased_properties
               (index (Option.get propholder)));

          state_sell_prop updated_player (Option.get propholder)
        end
        else begin
          print_endline
            ("Invalid Selection. End of turn for " ^ Player.name updated_player);
          updated_player
        end
      in
      (* print_endline (string_of_int (location updated_player)); print_endline
         (string_of_int (index (Option.get propholder)))*)
      (playernow, s.purchased_properties, s.money_jar)
  | _ ->
      print_endline ("End of turn for " ^ Player.name updated_player);
      (updated_player, s.purchased_properties, s.money_jar)

let rec take_turns (s : state) : state =
  match s.players with
  | [] -> { players = []; purchased_properties = []; money_jar = 0 }
  | h :: t -> (
      match one_turn s h with
      | p, new_s, m ->
          if Player.cash p < 0 then
            {
              players =
                (take_turns
                   { players = t; purchased_properties = new_s; money_jar = m })
                  .players;
              purchased_properties = new_s;
              money_jar = m;
            }
          else
            {
              players =
                p
                :: (take_turns
                      {
                        players = t;
                        purchased_properties = new_s;
                        money_jar = m;
                      })
                     .players;
              purchased_properties = new_s;
              money_jar = m;
            })
