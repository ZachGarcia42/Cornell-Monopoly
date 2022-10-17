open Game
open Monopoly
open Random
open Player
open Tile
open Board




(** The default starting money for each player. *)
let starting_money = 1500



(** [inform_player playerinfo] prints a terminal output that informs players of
    essential information they need to begin each turn, including how much money
    they have and the property they are currently at. *)
let inform_player (playerinfo : player) =


  print_endline ("Starting turn for player " ^ name playerinfo);

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


let rec is_in_list a lst = 
  match lst with 
  |[] -> false 
  |h :: t -> if h = a then true else is_in_list a t


(* Purchases the property and updates the player's values*)
let purchase_property 
(player: player)(property_val : int) = 
charge player property_val
  
(** [one_turn player] represents a single turn for [player]. Returns the updated
    player record after turn has been completed. *)
let rec one_turn (player : player) =
  print_endline
    ("---------------------Starting turn for player " ^ name player
   ^ "---------------------");
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

      (* TODO: I have only inserted a default value for the price of the property. 
         We should try to get the value of the property given the name of the property (as a string) *)
      let player = purchase_property player 50 in 
      print_endline "Congratulations, you have just bought a property! ";
      print_endline ("End of turn for " ^ name player);
      player
  | _ ->
      print_endline ("End of turn for " ^ name player);
      player



let rec take_turns (players : player list) : player list =
  match players with
  | [] -> []
  | h :: t -> one_turn h :: take_turns t

(** [end_conditions] is true if at least one of the game-ending conditions is
    true, false otherwise. (PLACEHOLDER) *)
let end_conditions = false (* TODO: check game ending conditions. *)

(** [game_loop players turn] repeatedly rotates through players' turns until the
    game ends, where [turn] represents which round of turns the game is on. The
    majority of the game will be spent in this state.*)
let rec game_loop (players : player list) (turn : int) =
  print_endline "";
  print_endline
    ("=======================Starting turn number " ^ string_of_int turn
   ^ " for all players=======================");
  print_endline "";
  let updated_players = take_turns players in
  if end_conditions then () else game_loop updated_players (turn + 1)

(** [print_player_names players] prints out the names of all players in order. *)
let rec print_player_names players =
  match players with
  | [] -> ()
  | [ h ] -> print_endline (name h)
  | h :: t ->
      print_string (name h ^ ", ");
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
  print_string "We begin our game of Cornellopoly with the following players: ";
  print_player_names players_lst;
  game_loop players_lst 1;
  ANSITerminal.print_string [ ANSITerminal.green ] "End of game."

let () = main ()