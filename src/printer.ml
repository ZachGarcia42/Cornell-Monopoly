let slow = 0.05
let med = 0.025
let fast = 0.01

(* flag that toggles type printing. If you're playing the game as normal, set to
   false. If you're debugging and just want to see the output faster, set to
   true. *)
let debugging = true

let rec print_chars ?speed:(setting = med) = function
  | [] -> ()
  | h :: t ->
      print_char h;
      if debugging then Unix.sleepf 0. else Unix.sleepf setting;
      flush stdout;
      print_chars t

let print_typed_string (s : string) =
  let char_list = List.of_seq (String.to_seq s) in
  print_chars char_list;
  print_endline ""