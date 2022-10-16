open Game
open Monopoly
open Random


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
    let roll = string_of_int ((Random.int 5) + 1) in 
    let tell_roll = "Your roll is " ^ roll in 
    print_endline tell_roll

let () = main () 