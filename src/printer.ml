let slow = 0.05
let med = 0.025
let fast = 0.01

let rec print_chars ?speed:(setting = med) = function
  | [] -> ()
  | h :: t ->
      print_char h;
      Unix.sleepf setting;
      flush stdout;
      print_chars t

let print_typed_string (s : string) =
  let char_list = List.of_seq (String.to_seq s) in
  print_chars char_list;
  print_endline ""