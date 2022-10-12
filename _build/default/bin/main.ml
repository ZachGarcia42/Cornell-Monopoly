open Game
open Monopoly


let rec main () = 
  ANSITerminal.print_string [ANSITerminal.green]
  "Welcome to Cornell Monopoly! In this game, you'll get to play a
  version of the popular board game Monopoly while learning a lot about Cornell University!\n";
  print_endline" ";
  print_endline "Please enter your name: ";
  print_string "> ";
  match read_line () with 
  | name -> ()

let () = main () 