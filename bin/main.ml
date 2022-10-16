open Game
open Monopoly
open Random
open Player

let inform_player (playerinfo: player) = 
  let money = cash playerinfo in 
    let position = position playerinfo in 
      print_int money;
      print_int position;
      ()

let rec main () = 
  ANSITerminal.print_string [ANSITerminal.green]
  "Welcome to Cornellopoly! In this game, you'll get to play a
  version of the popular board game Monopoly while learning a lot about Cornell University!\n";
  print_endline" ";
  print_endline "Please enter your name: ";
  print_string "> ";
  match read_line () with 
  | name -> 
    print_endline "Hello! ";
    let player_info = init_player name 0 1500 in 
    inform_player player_info;
    let roll = string_of_int ((Random.int 5) + 1) in 
    let tell_roll = "Your roll is " ^ roll in 
    print_endline tell_roll

let () = main () 