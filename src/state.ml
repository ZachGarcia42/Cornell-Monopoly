open Player
open Tile
open Board
open Chance
open Property
open Chest
open Printer

let starting_money = 1500

type state = {
  players : player list;
  purchased_properties : int list;
  money_jar : int;
}

let command_list =
  [
    "Press P to attempt to purchase a property";
    "Press C to draw a chance card";
    "Press H to draw a community chest card";
    "Press T to pay a tax";
    "Press Q to quit";
    "Type Collect to receive rewards on Free Parking";
    "Press S to sell a property";
  ]

let player_list state = state.players
let purchased_properties state = state.purchased_properties
let money_jar state = state.money_jar

let init_state (players : player list) props money : state =
  { players; purchased_properties = props; money_jar = money }

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
let remove_properties state property =
  init_state (player_list state)
    (List.filter
       (fun p -> if p = property then false else true)
       (purchased_properties state))
    (money_jar state)

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

(** [make_line size line] is a string with horizontal dashes proportional to the
    magnitude of [size]. Starts with [line]. *)
let rec make_line (size : int) (line : string) =
  if size = 0 then line else make_line (size - 1) (line ^ "_")

(** [make_space size line] is a string with spaces proportional to the magnitude
    of [size]. Starts with [line] *)
let rec make_space (size : int) (space : string) =
  if size = 0 then space else make_space (size - 1) (space ^ " ")

(** [inform_player s player_info current] tells [player] important info at the
    beginning of their turn, including what they rolled, how much money they
    have, their new position, and if they're on a property, how much that
    property costs. *)
let inform_player (s : state) (player : player) (current_tile : Tile.tile)
    (roll : int) : unit =
  print_typed_string
    ("You take the pair of dice in your hands, take a deep breath, then cast \
      them onto the table. Your heart hangs in your throat as you wait for the \
      pair of numbers to settle...You rolled a " ^ string_of_int roll ^ "!");

  let money = cash player in

  print_typed_string ("Bank Account balance: $" ^ string_of_int money);
  print_typed_string ("New position: " ^ tileName current_tile);

  match current_tile with
  | Property p ->
      let top = "" in
      let name_length = String.length (tileName current_tile) in
      let top = make_line 50 top in
      print_endline top;
      let spacer = "" in
      let name_spacer = make_space ((50 - name_length) / 2) spacer in

      print_endline
        ("|" ^ name_spacer ^ tileName current_tile ^ name_spacer ^ "|");
      let property_spacer =
        make_space
          ((50
           - String.length
               ("Property value: $ "
               ^ string_of_int (Tile.get_price current_tile)))
          / 2)
          spacer
      in
      print_endline
        ("|" ^ property_spacer ^ "Property value: $ "
        ^ string_of_int (Tile.get_price current_tile)
        ^ property_spacer ^ "|");
      let color_spacer =
        make_space
          ((50
           - String.length
               ("Color: " ^ Property.string_of_set (Property.color p)))
          / 2)
          spacer
      in
      print_endline
        ("|" ^ color_spacer ^ "Color: "
        ^ Property.string_of_set (Property.color p)
        ^ color_spacer ^ "|");
      if is_property_owned p s.players then
        let owner_spacer =
          make_space
            ((50 - String.length ("Owner: " ^ find_owner p s.players)) / 2)
            spacer
        in
        print_endline
          ("|" ^ owner_spacer ^ "Owner: " ^ find_owner p s.players
         ^ owner_spacer ^ "|")
      else
        let owner_spacer =
          make_space ((50 - String.length "Owner: None") / 2) spacer
        in
        print_endline ("|" ^ owner_spacer ^ "Owner: None" ^ owner_spacer ^ "|");
        print_endline top
  | _ -> ()

let rec init_players players_lst =
  print_typed_string "Please enter your name: ";
  print_string "> ";

  match read_line () with
  | name -> (
      let new_player = init_player name starting_money in

      let updated_players = players_lst @ [ new_player ] in
      print_typed_string
        ("Successfully created new player named " ^ Player.name new_player);
      print_typed_string "Enter another player? Enter 'Yes' or 'No'";

      match String.lowercase_ascii (read_line ()) with
      | "yes" | "y" -> init_players updated_players
      | "no" | "n" -> updated_players
      | _ ->
          print_typed_string "I didn't understand that";
          updated_players)

(*[player_name player str] matches a string [str] to a property that player
  [player] has purchased. Returns None if [str] is not the name of a player's
  property.*)
let rec player_name_to_property (player : player) str =
  match properties player with
  | [] -> None
  | h :: t ->
      if Monopoly.parse_user_input (Property.name h) = str then Some h
      else player_name_to_property (Player.sell_property player h) str

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
      print_typed_string "Collect $200 here! You can't purchase this";
      charge player 0
  | Property x ->
      print_typed_string
        "Congratulations! You have successfully purchased this property.";
      buy_property player x
  | IncomeTax ->
      print_typed_string
        "Sorry, this is a tax! You cannot purchase \n\
        \    this property! Deducting 200 to pay the tax! ";
      charge player 200
  | LuxuryTax ->
      print_typed_string
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

let unlock_comm_chest_card (player: player) property = 
  match property with 
  |CommunityChest c -> 
    print_endline (name c);
    let dest = Chest.destination c in 
    if dest <> "Current" then 
      let new_pos = get_pos board dest 0 in 
      print_typed_string ("You have advanced to " ^ dest);

      let did_player_pass_go = 
        if Monopoly.player_passed_go (get_pos board (tileName property) 0 ) (get_pos board dest 0)
        then 
          pay player 200 else player ;
      in 

      Player.move_to did_player_pass_go new_pos 
    else 
      let payment = Chest.payment c in 
      if payment < 0 then charge player (-1 * payment) else 
        pay player payment
  |_ -> 
    print_endline "This is not a Community Chest Card!";
    player

let unlock_chance_card (player : player) property =
  match property with
  | Chance c ->
      let dest = Chance.destination c in
      let ctype = Chance.name c in
      let price = Chance.price c in
      if ctype = "Chance: Advancement" then (
        let new_pos = get_pos board dest 0 in
        print_typed_string ("You have advanced to " ^ dest);
        let did_player_pass_go = 
          if Monopoly.player_passed_go (get_pos board (tileName property) 0 ) (get_pos board dest 0)
          then 
            pay player 200 else player ;
        in 
        Player.move_to did_player_pass_go new_pos)
      else if ctype = "Chance: Money Made" then pay player price
      else if ctype = "Chance: Payment Required" then charge player price
      else Player.add_get_out_card player
  | _ ->
      print_typed_string "This is not a Chance Card!";
      charge player 0

(** [rent_charge_inform s p player] informs the current player whose turn it of
    the owner of the property [p] they landed on and how much they are being
    charged as rent *)
let rent_charge_inform (s : state) (p : Property.t) =
  let owner = find_owner p s.players in
  print_typed_string ("This property is owned by " ^ owner);
  print_typed_string
    ("You are being charged "
    ^ string_of_int (Property.price p)
    ^ "for the privilege of staying on their properties. You may not take any \
       other actions, press any key to continue.")

(** [charged_player player property] is the updated player after they've been
    charged for landing on [property]*)
let charged_player (player : Player.player) (property : Property.t) =
  Player.charge player (Property.price property)

(* [prompt_next_action] prompts the player's next key-press based on what tile
   type they are currently on. *)
let prompt_next_action state tile player =
  match tile with
  | Go -> print_typed_string "You are on the Go tile"
  | Property p ->
      if is_property_owned p state.players then rent_charge_inform state p
      else (
        print_typed_string
          ("This property costs $" ^ string_of_int (Property.price p));
        print_typed_string
          "Attempt to purchase this property? Enter 'P' if you wish to do so")
  | CommunityChest h->
      print_typed_string
        "You can draw a Community Chest Card. Press 'H' to proceed"
  | IncomeTax | LuxuryTax ->
      print_typed_string "You need to pay your taxes! Enter 'T' to continue."
  | Chance _ ->
      print_typed_string
        "You have landed on a Chance Square! Enter 'C' to proceed."
  | JustVisiting ->
      print_typed_string
        "You are just visiting your old Dyson pal (who recently committed \n\
         financial fraud) in jail. No action needs to be taken – enter any \
         other key to continue. "
  | FreeParking ->
      print_typed_string
        "You have landed on free parking! Enter 'Collect' to collect your \
         rewards!"
  | _ ->
      print_typed_string
        "Enter 'Q' to quit the game, or do nothing (enter any other key)."

let rec one_turn (s : state) (player : player) =
  print_endline
    ("---------------------Starting turn for player " ^ Player.name player
   ^ "---------------------");
  let roll = Random.int 6 + 1 + (Random.int 6 + 1) in

  let old_position = location player in
  let new_position = (old_position + roll) mod List.length Board.board in

  let updated_player = move_to player new_position in

  print_typed_string
    ("You are starting this turn on "
    ^ Tile.tileName (List.nth Board.board old_position));

  let updated_player =
    if Monopoly.player_passed_go old_position new_position then (
      print_typed_string "You have passed Go! You win $200";
      pay updated_player 200)
    else updated_player
  in

  let current_tile = List.nth Board.board new_position in

  inform_player s player current_tile roll;

  (* [updated_player] is the new player identifier after they have been charged
     rent for landing on their current location property, if applicable. *)
  let updated_player =
    match List.nth Board.board new_position with
    | Property p ->
        if is_property_owned p s.players then charged_player updated_player p
        else updated_player
    | _ -> updated_player
  in

  prompt_next_action s current_tile updated_player;
  print_typed_string "Enter 'S' to sell a property";

  match Monopoly.parse_user_input (read_line ()) with
  | "P" ->
      if check_properties s new_position then (
        print_typed_string "Sorry! This property has already been purchased.";
        print_typed_string ("End of turn for " ^ Player.name updated_player);
        (updated_player, s.purchased_properties, s.money_jar))
      else
        let property_price =
          match List.nth board (location updated_player) with
          | Property p -> Property.price p
          | _ -> 0
        in
        if Player.cash updated_player < property_price then (
          print_typed_string
            "Sorry, you do not have enough money to purchase this property!";
          (updated_player, s.purchased_properties, s.money_jar))
        else
          let player_purchased =
            purchase_property updated_player current_tile
          in

          print_typed_string ("End of turn for " ^ Player.name player_purchased);
          ( player_purchased,
            add_properties s.purchased_properties (location updated_player),
            s.money_jar )
  | "C" ->
      print_typed_string "Drawing a chance card...";

      (* TODO: Implement drawing a chance card. *)
      let next_update =
        unlock_chance_card updated_player (List.nth Board.board new_position)
      in
      (next_update, s.purchased_properties, s.money_jar)
  | "H" ->
      print_typed_string "Drawing a community chest card ...";
      let next_update = unlock_comm_chest_card updated_player (List.nth Board.board new_position) in 

      (next_update, s.purchased_properties, s.money_jar)
  | "T" ->
      let taxable_tile = List.nth board (location updated_player) in
      let tax_amt = Tile.get_price (List.nth board (location updated_player)) in
      let player_paid = pay_tax player taxable_tile in
      if Player.cash player < 0 then
        print_typed_string
          (Player.name player_paid
         ^ " has gone bankrupt, Cornell's overwhelming costs have proved to be \
            too much for them!")
      else print_typed_string ("End of turn for " ^ Player.name player_paid);
      let updated_player_position =
        Player.move_to player_paid (location updated_player)
      in
      (updated_player_position, s.purchased_properties, s.money_jar + tax_amt)
  | "Q" ->
      print_typed_string
        "Thank you for playing Cornellopoly! We hope you had fun!";
      exit 0
  | "Collect" ->
      print_typed_string
        ("Congrats, You have reaped the rewards of landing on free parking!"
       ^ string_of_int s.money_jar ^ "will be added to your bank account.");
      (updated_player, s.purchased_properties, 0)
  | "S" ->
      print_typed_string
        "Pick from the following properties, or enter any other value to exit:";
      print_typed_string (string_list_properties player);
      let inp = Monopoly.parse_user_input (read_line ()) in
      let propholder = player_name_to_property updated_player inp in
      let playernow =
        if
          propholder != None
          && has_property updated_player (Option.get propholder)
        then begin
          print_endline
            (inp ^ " sold t. = End of turn for " ^ Player.name updated_player);
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
    (* Check if the player has forgotten to pay a tax here - if so charge the player*)
      let possible_update = 
        match List.nth board (location updated_player) with 
        |IncomeTax -> charge updated_player 200 
        |LuxuryTax -> charge updated_player 100 
        |_ -> charge updated_player 0
      in 
        print_endline ("End of turn for " ^ Player.name updated_player);
      (possible_update, s.purchased_properties, s.money_jar)

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

