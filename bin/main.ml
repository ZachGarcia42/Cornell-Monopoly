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
    "Press Q to quit";
    "Press S to sell a property";
    "Press H at any time for help";
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

let display_board (board : Tile.tile list) (pos : int) =
  print_endline "";
  print_endline
    "You glance around at your surroundings and see the following several \
     locations behind and in front of you. ";

  let printed_tiles =
    [
      List.nth board (Monopoly.convert (pos - 2) (List.length board));
      List.nth board (Monopoly.convert (pos - 1) (List.length board));
      List.nth board (Monopoly.convert pos (List.length board));
      List.nth board (Monopoly.convert (pos + 1) (List.length board));
      List.nth board (Monopoly.convert (pos + 2) (List.length board));
    ]
  in

  let rec print_tiles printed_tiles =
    match printed_tiles with
    | [] -> " | "
    | h :: t -> " | " ^ tileName h ^ print_tiles t
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
  print_typed_string
    ("New position: " ^ if in_jail player then "Jail" else tileName current_tile);

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
let rec init_players players_lst counter flag =
  if counter = 0 || flag = true then begin
    print_typed_string "Please enter your name: ";
    print_string "> ";

    match read_line () with
    | "" ->
        print_typed_string "Invalid name! Your name cannot be blank!";
        init_players players_lst counter flag
    | name ->
        let new_player = init_player name starting_money in
        print_typed_string
          ("Successfully created new player named " ^ Player.name new_player);
        let updated_players = players_lst @ [ new_player ] in
        init_players updated_players (counter + 1) false
  end
  else if counter = 1 then begin
    print_typed_string "Please enter a second player: ";
    print_string "> ";

    match read_line () with
    | "" ->
        print_typed_string "Invalid name! Your name cannot be blank!";
        init_players players_lst counter flag
    | name -> (
        let new_player = init_player name starting_money in
        print_typed_string
          ("Successfully created new player named " ^ Player.name new_player);
        let updated_players = players_lst @ [ new_player ] in

        print_typed_string "Enter another player? Enter 'Yes' or 'No'";

        match String.lowercase_ascii (read_line ()) with
        | "yes" | "y" -> init_players updated_players (counter + 1) true
        | "no" | "n" -> updated_players
        | _ ->
            print_typed_string "I didn't understand that";
            init_players updated_players counter flag)
  end
  else
    let updated_players = players_lst in
    print_typed_string "Enter another player? Enter 'Yes' or 'No'";

    match String.lowercase_ascii (read_line ()) with
    | "yes" | "y" -> init_players updated_players (counter + 1) true
    | "no" | "n" -> updated_players
    | _ ->
        print_typed_string "I didn't understand that";
        init_players updated_players counter flag

(** [rent_charge_inform s p player] informs the current player whose turn it of
    the owner of the property [p] they landed on and how much they are being
    charged as rent *)
let rent_charge_inform (s : state) (p : Property.t) (pl : player) =
  let owner = find_owner p (player_list s) in

  if Player.name pl <> owner then (
    print_typed_string ("This property is owned by " ^ owner);
    print_typed_string
      ("You are being charged $"
      ^ string_of_int (Property.price p)
      ^ " for the privilege of staying on their properties. You may not take \
         any other actions related to this property."))
  else
    print_typed_string
      "This is your property! You don't have to pay any rent fees"

(* [prompt_next_action] prompts the player's next key-press based on what tile
   type they are currently on. *)
let prompt_next_action state tile player =
  if in_jail player then
    print_typed_string
      "You are in jail with your old Dyson pal (who recently committed \n\
       financial fraud). No action needs to be taken – enter any other key to \
       continue. "
  else
    match tile with
    | Go -> print_typed_string "You are on the Go tile"
    | Property p ->
        if is_property_owned p (player_list state) then
          rent_charge_inform state p player
        else (
          print_typed_string
            ("This property costs $" ^ string_of_int (Property.price p));
          print_typed_string
            "Attempt to purchase this property? Enter 'P' if you wish to do so")
    | CommunityChest h ->
        print_typed_string
          "You have landed on Community Chest! Drawing Community Chest card..."
    | IncomeTax ->
        print_typed_string
          "You need to pay your taxes! Martha snatched $200 from your account."
    | LuxuryTax ->
        print_typed_string
          "You need to pay your taxes! Martha snatched $100 from your account."
    | Chance _ ->
        print_typed_string
          "You have landed on a Chance Square! Drawing Chance card..."
    | JustVisiting ->
        print_typed_string
          "You are just visiting your old Dyson pal (who recently committed \n\
           financial fraud) in jail. No action needs to be taken – enter any \
           other key to continue. "
    | FreeParking ->
        print_typed_string
          "You have landed on free parking! Time to reap the rewards! $100 \
           will be added to your bank account."
    | GoToJail -> print_typed_string "You are ordered to go to jail! "
(*| _ -> print_typed_string "Enter 'Q' to quit the game, or do nothing (enter
  any other key)." *)

(**[has_goojf p] is true iff p has at least one get out of jail free card*)
let has_goojf player = cards player > 0

let yes_no_helper = ref (fun s -> true)

let yes_no_input s =
  match Monopoly.parse_user_input s with
  | "Y" | "YES" -> true
  | "N" | "NO" -> false
  | _ ->
      print_endline "Please enter \"Y\" or \"N\".";
      !yes_no_helper (read_line ())

let x = yes_no_helper := yes_no_input

let rev_sort_assoc_list lst =
  List.rev
    (List.sort
       (fun x y -> if snd x > snd y then 1 else if snd x < snd y then -1 else 0)
       lst)

let rec get_pl lst =
  match lst with
  | [] -> []
  | h :: t -> fst h :: get_pl t

let rec cash_to_players players =
  match players with
  | [] -> []
  | h :: t -> (Player.name h, Player.cash h) :: cash_to_players t

(* Prints the players standings*)
let print_player_standings (players : player list) =
  let standings_list = rev_sort_assoc_list (cash_to_players players) in
  let rankings = get_pl standings_list in
  rankings

let rec print_standings lst (cashlst : (string * int) list) =
  print_endline " ----- LEADERBOARD ----- ";
  for i = 0 to List.length lst - 1 do
    print_endline
      ("Rank "
      ^ string_of_int (i + 1)
      ^ " : " ^ List.nth lst i ^ " has " ^ "$"
      ^ string_of_int (List.assoc (List.nth lst i) cashlst))
  done

let handle_card player =
  print_endline
    "You are in Jail and have a Get out of Jail Free Card. Would you like to \
     use it? (Y/N)";
  if yes_no_input (read_line ()) then use_card player else jail_turn player

(**[handle_card p] represents the player after they choose whether to use a get
   out of jail free card or not*)

(** Replaces the [players] list's corresponding [updated_player] *)
let rec update_player_list (updated_player : player) (players : player list) =
  match players with
  | [] -> []
  | h :: t ->
      if Player.name h = Player.name updated_player then updated_player :: t
      else h :: update_player_list updated_player t

(** Creates a state that one_turn returns from the currently available changed
    data. *)
let reconstruct_state (updated_player : player)
    (purchased_properties : int list) (state : state) : player * state =
  let new_players_list =
    update_player_list updated_player (State.player_list state)
  in
  ( updated_player,
    init_state new_players_list purchased_properties (money_jar state) )

(** [one_turn player] represents a single turn for [player]. Returns the updated
    player and state with the updated player inside after turn has been
    completed. *)
let rec one_turn (s : state) (player : player) plist =
  print_endline "";
  print_endline
    ("---------------------Starting turn for player " ^ Player.name player
   ^ "---------------------");
  let die1 = Random.int 6 + 1 in
  let die2 = Random.int 6 + 1 in
  let doubles = die1 = die2 in
  let roll = die1 + die2 in

  let old_position = location player in
  let roll_position = (old_position + roll) mod List.length Board.board in

  let updated_player =
    match player with
    | p when in_jail p && doubles -> leave_jail p
    | p when in_jail p && has_goojf p -> handle_card p
    | p when in_jail p && turns_in_jail p < 3 && not doubles -> jail_turn p
    | p when in_jail p && turns_in_jail p >= 3 && not doubles ->
        print_endline "You were charged $50 bail to leave jail.";
        move_to (leave_jail (charge p 50)) roll_position
    | _ -> move_to player roll_position
  in

  print_typed_string
    ("You are starting this turn on "
    ^
    if in_jail updated_player then "Jail"
    else Tile.tileName (List.nth Board.board old_position));

  let new_position = location updated_player in
  let updated_player =
    if Monopoly.player_passed_go old_position new_position then (
      print_typed_string "You have passed Go! You win $200";
      pay updated_player 200)
    else updated_player
  in

  let current_tile = List.nth Board.board new_position in

  inform_player s updated_player current_tile roll;

  (* print_standings (print_player_standings plist); *)
  display_board Board.board new_position;

  (* [updated_player] is the new player identifier after they have been charged
     rent for landing on their current location property, if applicable. *)
  let updated_player =
    match List.nth Board.board new_position with
    | Property p ->
        if is_property_owned p (s |> player_list) then (
          print_typed_string "Updating player records...";
          updated_player)
        else updated_player
    | _ -> updated_player
  in

  prompt_next_action s current_tile updated_player;
  let prompt_if_not_jailed =
    match current_tile with
    | GoToJail -> ()
    | _ ->
        print_typed_string "Enter 'S' to sell a property";
        print_typed_string "Enter 'H' for help";
        print_typed_string
          "Or enter any other key to do nothing and continue on. "
  in

  prompt_if_not_jailed;

  let new_player =
    match current_tile with
    | CommunityChest h ->
        unlock_comm_chest_card updated_player
          (List.nth Board.board new_position)
          plist
    | Chance _ ->
        unlock_chance_card updated_player (List.nth Board.board new_position)
    | IncomeTax -> charge updated_player 200
    | LuxuryTax -> charge updated_player 100
    | FreeParking -> pay updated_player 100
    | GoToJail ->
        print_typed_string "Moving you to Jail....";
        let new_pos = get_pos board (tileName JustVisiting) 0 in
        let updated_player =
          Player.go_to_jail (Player.move_to updated_player new_pos)
        in
        updated_player
    | Property p ->
        if
          is_property_owned p (player_list s)
          && Player.name updated_player <> find_owner p (player_list s)
        then (
          print_endline ("Charging " ^ Player.name updated_player ^ "...");
          let rentable_tile = List.nth board (location updated_player) in
          let rent = Tile.get_price rentable_tile in
          let player_paid = charge updated_player rent in

          if Player.cash player < 0 then
            print_typed_string
              (Player.name player_paid
             ^ " has gone bankrupt, Cornell's overwhelming costs have proved \
                to be too much for them!")
          else print_typed_string ("End of turn for " ^ Player.name player_paid);
          let updated_player_position =
            Player.move_to player_paid (location updated_player)
          in
          updated_player_position)
        else updated_player
    | _ -> updated_player
  in

  match Monopoly.parse_user_input (read_line ()) with
  | "P" -> begin
      match current_tile with
      | Property _ ->
          if check_properties s new_position then (
            print_typed_string
              "Sorry! This property has already been purchased.";
            print_typed_string ("End of turn for " ^ Player.name new_player);
            reconstruct_state new_player (purchased_properties s) s)
          else
            let property_price =
              match List.nth board (location new_player) with
              | Property p -> Property.price p
              | _ -> 0
            in
            if Player.cash new_player < property_price then (
              print_typed_string
                "Sorry, you do not have enough money to purchase this property!";
              reconstruct_state new_player (purchased_properties s) s)
            else
              let player_purchased =
                purchase_property new_player current_tile
              in

              print_typed_string
                ("End of turn for " ^ Player.name player_purchased);
              reconstruct_state player_purchased
                (add_properties (purchased_properties s) (location new_player))
                s
      | _ ->
          print_typed_string "Sorry, this property cannot be purchased!";
          reconstruct_state updated_player (purchased_properties s) s
    end
  | "Q" ->
      print_typed_string
        "Thank you for playing Cornellopoly! We hope you had fun!";
      exit 0
  | "S" ->
      if List.length (Player.properties new_player) > 0 then (
        print_typed_string
          "Pick from the following properties, or enter any other value to \
           exit:";
        print_typed_string (string_list_properties player);
        let inp = Monopoly.parse_user_input (read_line ()) in
        let propholder = player_name_to_property new_player inp in
        let playernow =
          if
            propholder <> None
            && has_property new_player (Option.get propholder)
          then begin
            print_endline
              (inp ^ " sold t. = End of turn for " ^ Player.name new_player);

            (* ignore (remove_properties s (index (Option.get propholder))); *)
            sell_property new_player (Option.get propholder)
          end
          else begin
            print_endline
              ("Invalid Selection. End of turn for " ^ Player.name new_player);
            new_player
          end
        in
        (* print_endline (string_of_int (location new_player)); print_endline
           (string_of_int (index (Option.get propholder)))*)
        reconstruct_state playernow (purchased_properties s) s)
      else (
        print_typed_string "Sorry, you currently don't own any properties";
        reconstruct_state new_player (purchased_properties s) s)
  | "H" ->
      let help =
        print_endline
          "Hello! Here's a brief overview of how the game works: At the \
           beginning of each turn, we roll a pair of die for you and advance \
           your character that many spaces on the game board. We give you \
           useful information like your current bank account balance, the \
           current square you're on, and a few tiles around you. Then, you are \
           prompted to enter your next action based on what square you're on. \
           Note that you must enter exactly what's prompted in most cases, \
           although you will still be charged for rent, taxes, and other \
           things regardless of what you enter (so tax evasion isn't \
           possible)! Hope this helps!"
      in
      help;
      reconstruct_state new_player (purchased_properties s) s
  | "Help" ->
      display_commands command_list;
      (* This does not work btw *)
      reconstruct_state new_player (purchased_properties s) s
  | _ -> (
      (* If player is on another player's property, pays that player. *)
      match current_tile with
      | Property p ->
          (* Returns the owner of property [p]. Returns the first player in the
             list of players if the owner isn't found (should never happen) *)
          let rec find_owner p players =
            match players with
            | [] -> List.nth players 0
            | h :: t -> if Player.has_property h p then h else find_owner p t
          in

          let rec pay_player_in_list player_to_be_paid amt players =
            match players with
            | [] -> []
            | h :: t ->
                if Player.name h = Player.name player_to_be_paid then
                  Player.pay player_to_be_paid amt :: t
                else h :: pay_player_in_list player_to_be_paid amt t
          in

          (* Accepts and returns a game state, the only difference being that
             [player_to_be_paid] gets paid [amt]*)
          let rec state_with_paid_player (state : state)
              (player_to_be_paid : player) (amt : int) =
            let players = State.player_list state in
            let new_players_list =
              pay_player_in_list player_to_be_paid amt players
            in
            init_state new_players_list
              (State.purchased_properties state)
              (State.money_jar state)
          in

          if is_property_owned p (player_list s) then (
            let property_price = Property.price p in
            let property_owner = find_owner p (player_list s) in
            if Player.name property_owner <> Player.name new_player then
              print_endline
                (Player.name property_owner ^ " was paid "
                ^ string_of_int property_price
                ^ " for rent!");

            print_endline ("End of turn for " ^ Player.name new_player);

            ( new_player,
              state_with_paid_player
                (snd (reconstruct_state new_player (purchased_properties s) s))
                property_owner property_price ))
          else (
            print_endline ("End of turn for " ^ Player.name new_player);
            reconstruct_state new_player (purchased_properties s) s)
      | _ ->
          print_endline ("End of turn for " ^ Player.name new_player);
          reconstruct_state new_player (purchased_properties s) s)

(** Removes player [p] from [players] *)
let rec remove_player (p : player) (players : player list) =
  match players with
  | [] -> []
  | h :: t ->
      if Player.name h = Player.name p then t else h :: remove_player p t

(** Removes player [p] from [state] *)
let trimmed_state (p : player) (state : state) =
  let old_player_list = State.player_list state in
  let new_player_list = remove_player p old_player_list in
  let purchased_props = State.purchased_properties state in
  let money_jar_money = State.money_jar state in
  init_state new_player_list purchased_props money_jar_money

(** [take_turns players] represents the new player states after each player
    takes one turn*)
let rec take_turns (s : state) plist : state =
  match s |> player_list with
  | [] -> init_state [] [] 0
  | h :: t -> (
      match one_turn s h plist with
      | p, new_state ->
          if Player.cash p < 0 then
            let newer_state = trimmed_state p new_state in
            init_state
              (player_list (take_turns newer_state plist))
              (State.purchased_properties newer_state)
              (State.money_jar newer_state)
          else
            let tail_player_list_state = trimmed_state p new_state in
            let total_player_list =
              p :: player_list (take_turns tail_player_list_state plist)
            in
            init_state total_player_list
              (State.purchased_properties new_state)
              (State.money_jar new_state)
            (* | p, new_s, m -> if Player.cash p < 0 then init_state
               (player_list (take_turns (init_state t new_s m))) new_s m else
               init_state (p :: player_list (take_turns (init_state t new_s m)))
               new_s m) *))

(** [game_loop players turn] repeatedly rotates through players' turns until the
    game ends, where [turn] represents which round of turns the game is on. The
    majority of the game will be spent in this state.*)
let rec game_loop (game : state) (turn : int) purchased playerlst =
  if List.length playerlst = 0 then exit 0 else print_endline "";
  print_endline "";
  print_endline
    ("=======================Starting turn number " ^ string_of_int turn
   ^ " for all players=======================");
  print_endline "";
  print_standings (print_player_standings playerlst) (cash_to_players playerlst);
  let updated_game = take_turns game playerlst in
  let updated_playerlst = State.player_list updated_game in
  if end_conditions then ()
  else game_loop updated_game (turn + 1) purchased updated_playerlst

(** Entry point of the monopoly game. Calls helper functions to manage game
    initialization and players' turns, but does not actually do any processing
    itself. *)
let rec main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "Welcome to Cornellopoly! In this game, you'll get to play a \n\
     version of the popular board game Monopoly while learning a lot about \n\
     Cornell University!\n\
     Here are the commands that you can use: \n";
  print_endline " ";
  display_commands command_list;
  print_endline "";
  let open Random in
  Random.self_init ();
  let players_lst = init_players [] 0 false in
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