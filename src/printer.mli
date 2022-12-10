(** Handles printing strings slowly*)

val print_typed_string : string -> unit
(** [print_typed_string s] prints out the characters in string [s] as if being
    typed, instead of all at once. Otherwise, behaves the same as print_endline.
    OPTIONAL ARGUMENT: [speed] is an additional argument representing the length
    of the pause between printing each char, users can supply a float. 0.5 is
    slow while 0.1 is fast. *)
