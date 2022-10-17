open Game
open Monopoly
open Random
open Player
open Tile

let play_order = 0
let starting_money = 1500

(** Informs players at the beginning of each turn with how much money they have
    and the property they are currently at. *)
let inform_player (playerinfo : player) =
  print_endline
    ("---------------------Starting turn for player " ^ name playerinfo
   ^ "---------------------");
  let money = cash playerinfo in
  let position = position playerinfo in
  print_endline ("You currently have $" ^ string_of_int money);
  print_endline ("You are currently at " ^ tileName (List.nth board position));
  ()

(** [init_players ()] instantiates the number of desired players and adds them
    to the players list *)
let rec init_players players_lst =
  print_endline "Please enter your name: ";
  print_string "> ";
  match read_line () with
  | name -> (
      let new_player =
        init_player name (List.length players_lst) starting_money
      in
      let updated_players = players_lst @ [ new_player ] in
      print_endline
        ("Successfully created new player named " ^ Player.name new_player);
      print_endline "Enter another player? Enter 'Yes' or 'No'";
      match read_line () with
      | "Yes" -> init_players updated_players
      | "No" -> updated_players
      | _ ->
          print_endline "I didn't understand that";
          updated_players)

(** A single player's turn. Returns the updated player record after turn has
    been completed.*)
let rec one_turn (player : player) =
  let roll = string_of_int (Random.int 5 + 1) in
  let tell_roll = "Your roll is " ^ roll in
  print_endline tell_roll;
  print_endline "Your new position after moving is now ";
  let updated_player = move_to player (location player + int_of_string roll) in
  inform_player updated_player;
  print_endline
    "What would you like to do? Purchase this property (enter 'P') or do \
     nothing (enter any other key) ";
  print_string "> ";
  match read_line () with
  | "P" ->
      print_endline "Placeholder";
      player
  | _ ->
      print_endline "Placeholder";
      player

let rec take_turns (players : player list) : player list =
  match players with
  | [] -> []
  | h :: t -> one_turn h :: take_turns t

(** [end_conditions] is true if at least one of the game-ending conditions is
    true, false otherwise. (PLACEHOLDER) *)
let end_conditions = true

(** Repeatedly rotates through players' turns until the game ends. The majority
    of the game will be spent in this state.*)
let rec game_loop (players : player list) (turn : int) =
  print_endline
    ("Starting turn number " ^ string_of_int turn ^ " for all players...");
  let updated_players = take_turns players in
  if end_conditions then () else game_loop updated_players (turn + 1)

(** For testing *)
let rec print_player_names players =
  match players with
  | [] -> ()
  | h :: t ->
      print_endline (name h);
      print_player_names t

(** Entry point of the monopoly game. Calls helper functions to manage game
    initialization and players' turns, but does not actually do any processing
    itself. *)
let rec main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Welcome to Cornellopoly! In this game, you'll get to play a\n\
    \  version of the popular board game Monopoly while learning a lot about \
     Cornell University!\n";
  print_endline " ";
  let players_lst = init_players [] in
  print_endline "Done initializing";
  print_endline (string_of_int (List.length players_lst));
  print_player_names players_lst;
  game_loop players_lst 1;
  print_endline "End of game."

let () = main ()