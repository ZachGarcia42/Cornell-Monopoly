let rec print_chars = function
  | [] -> ()
  | h :: t ->
      print_char h;
      Unix.sleepf 0.025;
      flush stdout;
      print_chars t

let print_typed_string (s : string) =
  let char_list = List.of_seq (String.to_seq s) in
  print_chars char_list;
  print_endline ""