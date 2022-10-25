open Game
open State
open Monopoly
open Random
open Player
open Tile
open Board
open Property

let list_players = ref []

(** [end_conditions] is true if at least one of the game-ending conditions is
    true, false otherwise. (PLACEHOLDER) *)
let end_conditions = false (* TODO: check game ending conditions. *)

(** [game_loop players turn] repeatedly rotates through players' turns until the
    game ends, where [turn] represents which round of turns the game is on. The
    majority of the game will be spent in this state.*)
let rec game_loop (game : state) (turn : int) =
  print_endline "";
  print_endline
    ("=======================Starting turn number " ^ string_of_int turn
   ^ " for all players=======================");
  print_endline "";
  let updated_game = take_turns game in
  if end_conditions then () else game_loop updated_game (turn + 1)

(** [print_player_names players] prints out the names of all players in order. *)
let rec print_player_names players =
  match players with
  | [] -> ()
  | [ h ] -> print_endline (Player.name h)
  | h :: t ->
      print_string (Player.name h ^ ", ");
      print_player_names t

(* Displays the Cornellopoly Board on the terminal for the players to see.*)
let display_board (board : Tile.tile list) =
  let rec print_8 (board : Tile.tile list) (count : int) =
    match count with
    | 0 -> " | "
    | n ->
        let tl = List.nth board (8 - n) in
        " | " ^ tileName tl ^ print_8 board (count - 1)
  in
  let x = print_8 board 8 in
  let filler = String.make (String.length x - 10) ' ' in
  let rec print_next_12 (board : Tile.tile list) (count : int) =
    match count with
    | 8 -> print_endline ""
    (* (filler ^ "-----------") *)
    | n ->
        let tl = List.nth board (29 - n) in
        print_endline (filler ^ "-----------");
        print_endline (filler ^ tileName tl);
        print_next_12 board (count - 1)
  in

  print_endline "Here is your Cornellopoly Board: ";
  print_endline "";
  print_string x;
  print_next_12 board 20;
  ()

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
  let game_state = init_state players_lst in
  list_players := players_lst;
  print_string "We begin our game of Cornellopoly with the following players: ";
  print_player_names players_lst;
  print_endline "";
  display_board Board.board;
  game_loop game_state 1;
  ANSITerminal.print_string [ ANSITerminal.green ] "End of game."

let () = main ()