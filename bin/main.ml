open Game
open State
open Monopoly
open Player
open Tile
open Board
open Property

(** [end_conditions] is true if at least one of the game-ending conditions is
    true, false otherwise. (PLACEHOLDER) *)
let end_conditions = false (* TODO: check game ending conditions. *)

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

let rec print_chars = function
  | [] -> ()
  | h :: t ->
      print_char h;
      Unix.sleepf 0.05;
      flush stdout;
      print_chars t

(** [print_typed_string s] prints out the characters in string [s] as if being
    typed, instead of all at once. Otherwise, behaves the same as print_endline. *)
let print_typed_string (s : string) =
  let char_list = List.of_seq (String.to_seq s) in
  print_chars char_list;
  print_endline ""

(** Entry point of the monopoly game. Calls helper functions to manage game
    initialization and players' turns, but does not actually do any processing
    itself. *)
let rec main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Welcome to Cornellopoly! In this game, you'll get to play a\n\
    \  version of the popular board game Monopoly while learning a lot about \
     Cornell University!\n";
  print_endline " ";
  print_typed_string "Testing printing string";
  let open Random in
  Random.self_init ();
  let players_lst = init_players [] in
  let game_state = init_state players_lst in
  print_string "We begin our game of Cornellopoly with the following players: ";
  print_player_names players_lst;
  print_endline "";
  display_board_revised Board.board;
  game_loop game_state 1 [] players_lst;
  ANSITerminal.print_string [ ANSITerminal.green ] "End of game."

let () = main ()