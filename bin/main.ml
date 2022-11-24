open Game
open State
open Monopoly
open Player
open Tile
open Board
open Property
open Printer

(** [end_conditions] is true if at least one of the game-ending conditions is
    true, false otherwise. (PLACEHOLDER) *)
let end_conditions = false (* TODO: check game ending conditions. *)

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

let display_commands (cmdlist : string list) =
  for i = 0 to List.length cmdlist - 1 do
    print_endline (List.nth cmdlist i)
  done

(** [print_player_names players] prints out the names of all players in order. *)
let rec print_player_names players =
  match players with
  | [] -> ()
  | [ h ] -> print_endline (Player.name h)
  | h :: t ->
      print_string (Player.name h ^ ", ");
      print_player_names t

(* Displays the Cornellopoly Board on the terminal for the players to see.*)
let display_board_revised (board : Tile.tile list) =
  print_endline "Here is your Cornellopoly Board: ";
  let rec print_8 (board : Tile.tile list) (count : int) =
    match count with
    | 0 -> " | "
    | n ->
        let tl = List.nth board (8 - n) in
        " | " ^ tileName tl ^ print_8 board (count - 1)
  in
  print_endline (print_8 board 8);

  let print_sides (board : Tile.tile list) (idx : int) =
    let tl = tileName (List.nth board (47 - idx)) in

    let tl2 = tileName (List.nth board idx) in
    tl ^ "                           " ^ tl2
  in
  print_endline "";
  for i = 8 to 19 do
    print_endline (print_sides board i);
    print_endline "------"
  done

let display_board (board: Tile.tile list)(pos: int) = 
  print_endline "";
  print_endline "Here is a small view of where you are 
  right now, and what is around you"; 

  let printed_tiles = [
    List.nth board (Monopoly.convert (pos - 2) (List.length board));
    List.nth board (Monopoly.convert (pos - 1) (List.length board));
    List.nth board (Monopoly.convert pos (List.length board));
    List.nth board (Monopoly.convert (pos + 1) (List.length board));
    List.nth board (Monopoly.convert (pos + 2) (List.length board));
  ] in 
  
  let rec print_tiles (printed_tiles) = 
    match printed_tiles with 
    |[] -> " | "
    |h :: t -> 
       " | " ^ (tileName h) ^ print_tiles t
  
  in 
  let x = print_tiles printed_tiles in 
  print_endline (String.make (String.length x) '-');
  print_endline x;
  print_endline (String.make (String.length x) '-');
  ()

  

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
      if is_property_owned p (s |> player_list) then
        let owner_spacer =
          make_space
            ((50 - String.length ("Owner: " ^ find_owner p (s |> player_list)))
            / 2)
            spacer
        in
        print_endline
          ("|" ^ owner_spacer ^ "Owner: "
          ^ find_owner p (s |> player_list)
          ^ owner_spacer ^ "|")
      else
        let owner_spacer =
          make_space ((50 - String.length "Owner: None") / 2) spacer
        in
        print_endline ("|" ^ owner_spacer ^ "Owner: None" ^ owner_spacer ^ "|");
        print_endline top
  | _ -> ()

(** [init_players ()] instantiates the number of desired players and adds them
    to the players list *)
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

(** [rent_charge_inform s p player] informs the current player whose turn it of
    the owner of the property [p] they landed on and how much they are being
    charged as rent *)
let rent_charge_inform (s : state) (p : Property.t) (pl: player) =
  let owner = find_owner p (player_list s) in

  if ((Player.name pl) <> owner) then (
  print_typed_string ("This property is owned by " ^ owner);
  print_typed_string
    ("You are being charged  " 
    ^ string_of_int (Property.price p)
    ^ "for the privilege of staying on their properties. You may not take any \
       other actions, press any key to continue.") 
  )
  else 
    print_typed_string "This is your property! You don't have to pay any rent fees"

(* [prompt_next_action] prompts the player's next key-press based on what tile
   type they are currently on. *)
let prompt_next_action state tile player =
  match tile with
  | Go -> print_typed_string "You are on the Go tile"
  | Property p ->
      if is_property_owned p (player_list state) then rent_charge_inform state p player
      else (
        print_typed_string
          ("This property costs $" ^ string_of_int (Property.price p));
        print_typed_string
          "Attempt to purchase this property? Enter 'P' if you wish to do so")
  | CommunityChest h ->
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
  |GoToJail -> 
      print_typed_string 
      "You are ordered to go to jail! Press J to go to Jail"
  (*| _ ->
      print_typed_string
        "Enter 'Q' to quit the game, or do nothing (enter any other key)." *)

(** [one_turn player] represents a single turn for [player]. Returns the updated
    player record after turn has been completed. *)
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

  display_board Board.board new_position;

  (* [updated_player] is the new player identifier after they have been charged
     rent for landing on their current location property, if applicable. *)
  let updated_player =
    match List.nth Board.board new_position with
    | Property p ->
        if is_property_owned p (s |> player_list) then
          charged_player updated_player p
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
        (updated_player, purchased_properties s, money_jar s))
      else
        let property_price =
          match List.nth board (location updated_player) with
          | Property p -> Property.price p
          | _ -> 0
        in
        if Player.cash updated_player < property_price then (
          print_typed_string
            "Sorry, you do not have enough money to purchase this property!";
          (updated_player, purchased_properties s, money_jar s))
        else
          let player_purchased =
            purchase_property updated_player current_tile
          in

          print_typed_string ("End of turn for " ^ Player.name player_purchased);
          ( player_purchased,
            add_properties (purchased_properties s) (location updated_player),
            money_jar s )
  | "C" ->
      print_typed_string "Drawing a chance card...";

      (* TODO: Implement drawing a chance card. *)
      let next_update =
        unlock_chance_card updated_player (List.nth Board.board new_position)
      in
      (next_update, purchased_properties s, money_jar s)
  | "H" ->
      print_typed_string "Drawing a community chest card ...";
      let next_update =
        unlock_comm_chest_card updated_player
          (List.nth Board.board new_position)
      in

      (next_update, purchased_properties s, money_jar s)
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
      
      (updated_player_position, purchased_properties s, money_jar s + tax_amt)
  |"J" -> 
      print_typed_string "Moving you to Jail...."; 
      let new_pos = get_pos board (tileName GoToJail) 0 in 
      let new_update = 
        Player.move_to updated_player new_pos in 
      (new_update, purchased_properties s, money_jar s)
  | "Q" ->
      print_typed_string
        "Thank you for playing Cornellopoly! We hope you had fun!";
      exit 0
  | "Collect" ->
      print_typed_string
        ("Congrats, You have reaped the rewards of landing on free parking!"
        ^ string_of_int (money_jar s)
        ^ "will be added to your bank account.");
      (updated_player, purchased_properties s, 0)
  | "S" ->
      print_typed_string
        "Pick from the following properties, or enter any other value to exit:";
      print_typed_string (string_list_properties player);
      let inp = Monopoly.parse_user_input (read_line ()) in
      let propholder = player_name_to_property updated_player inp in
      let playernow =
        if
          propholder <> None
          && has_property updated_player (Option.get propholder)
        then begin
          print_endline
            (inp ^ " sold t. = End of turn for " ^ Player.name updated_player);

          (* ignore (remove_properties s (index (Option.get propholder))); *)
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
      (playernow, purchased_properties s, money_jar s)
  |"Help" -> 
    display_commands command_list; 
    (updated_player, purchased_properties s, money_jar s)
  | _ ->
      (* Check if the player has forgotten to pay a tax here - if so charge the
         player*)
      let possible_update =
        match List.nth board (location updated_player) with
        | IncomeTax -> charge updated_player 200
        | LuxuryTax -> charge updated_player 100
        | _ -> charge updated_player 0
      in
      print_endline ("End of turn for " ^ Player.name updated_player);
      (possible_update, purchased_properties s, money_jar s)

(** [take_turns players] represents the new player states after each player
    takes one turn*)
let rec take_turns (s : state) : state =
  match s |> player_list with
  | [] -> init_state [] [] 0
  | h :: t -> (
      match one_turn s h with
      | p, new_s, m ->
          if Player.cash p < 0 then
            init_state (player_list (take_turns (init_state t new_s m))) new_s m
          else
            init_state
              (p :: player_list (take_turns (init_state t new_s m)))
              new_s m)

(** [game_loop players turn] repeatedly rotates through players' turns until the
    game ends, where [turn] represents which round of turns the game is on. The
    majority of the game will be spent in this state.*)
let rec game_loop (game : state) (turn : int) purchased playerlst =
  if List.length playerlst = 0 then exit 0 else print_endline "";
  print_endline
    ("=======================Starting turn number " ^ string_of_int turn
   ^ " for all players=======================");
  print_endline "";
  let updated_game = take_turns game in
  let updated_playerlst = State.player_list updated_game in
  if end_conditions then ()
  else game_loop updated_game (turn + 1) purchased updated_playerlst

(** Entry point of the monopoly game. Calls helper functions to manage game
    initialization and players' turns, but does not actually do any processing
    itself. *)
let rec main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "Welcome to Cornellopoly! In this game, you'll get to play a\n\
    \  version of the popular board game Monopoly while learning a lot about \
     Cornell University!\n\
    \ Here are the commands that you can use: \n";
  print_endline " ";
  display_commands command_list;
  print_endline "";
  let open Random in
  Random.self_init ();
  let players_lst = init_players [] in
  let game_state = init_state players_lst [] 0 in
  print_typed_string
    "We begin our game of Cornellopoly with the following players: ";
  print_player_names players_lst;
  print_endline "";
  display_board Board.board 0;
  (* display_board_revised Board.board; *)
  game_loop game_state 1 [] players_lst;
  ANSITerminal.print_string [ ANSITerminal.green ] "End of game."

let () = main ()