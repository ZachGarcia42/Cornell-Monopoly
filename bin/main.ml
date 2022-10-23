open Game
open Monopoly
open Random
open Player
open Tile
open Board
open Property

(** The default starting money for each player. *)
let starting_money = 1500

let list_players = ref []

(** [end_conditions] is true if at least one of the game-ending conditions is
    true, false otherwise. (PLACEHOLDER) *)
let end_conditions = false (* TODO: check game ending conditions. *)


let get_price t = 
  match t with 
  |Property x -> Property.price x
  |IncomeTax -> -200
  |_ -> 0


(** [inform_player playerinfo roll] prints a terminal output that informs players of
    essential information they need to begin each turn, including how much money
    they have and the property they are currently at. *)
let inform_player (playerinfo : player)(roll: int) =

  print_endline ("Starting turn for player " ^ Player.name playerinfo);

  let money = cash playerinfo in
  let position = position playerinfo in
  let landed = List.nth board ((position + roll) mod 40) in 

  print_endline ("You currently have $" ^ string_of_int money);
  print_endline ("You are currently at " ^ tileName landed);
  print_endline("This property costs " ^ "$ " ^ string_of_int (get_price landed));

  landed


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


(* Purchases the property and updates the player's values*)
let purchase_property 
(player: player)(property)(roll: int)= 
match property with 
|Property x -> buy_property player x roll
|_ -> charge player 0 roll


(** [one_turn player] represents a single turn for [player]. Returns the updated
    player record after turn has been completed. *)
let rec one_turn (player : player) =
  print_endline
    ("---------------------Starting turn for player " ^ Player.name player
   ^ "---------------------");
  let roll = string_of_int (Random.int 5 + 1) in
  let tell_roll = "Your roll is " ^ roll in
  print_endline tell_roll;

  let old_position = location player in 
  let new_position = (old_position + int_of_string roll) mod 40 in 

  let updated_player = move_to player (new_position) in

  if Monopoly.player_passed_go old_position new_position then 
    print_endline "You have passed Go! You win $200";

  let if_player_passed_go =
     if Monopoly.player_passed_go old_position new_position 
      then (pay updated_player 200) 
      else updated_player 
  in
  
  let x = inform_player if_player_passed_go (int_of_string roll) in
  print_endline
    "What would you like to do? Purchase this property (enter 'P') or do \
     nothing (enter any other key) ";
  print_string "> ";
  match Monopoly.parse_user_input (read_line ()) with
  | "P" ->
      
      let player = purchase_property player x (int_of_string roll) in 

      print_endline "Congratulations, you have just bought a property! ";
      print_endline ("End of turn for " ^ Player.name player);
      player
  | _ ->
      let player = charge player 0 (int_of_string roll) in 
      print_endline ("End of turn for " ^ Player.name player);
      player



let rec take_turns (players : player list) : player list =
  match players with
  | [] -> []
  | h :: t -> one_turn h :: take_turns t


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
  | [ h ] -> print_endline (Player.name h)
  | h :: t ->
      print_string (Player.name h ^ ", ");
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
  list_players := players_lst;
  print_string "We begin our game of Cornellopoly with the following players: ";
  print_player_names players_lst;
  game_loop players_lst 1;
  ANSITerminal.print_string [ ANSITerminal.green ] "End of game."

let () = main ()