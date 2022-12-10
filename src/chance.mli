type t
(* The abstract type representing a Chance Card*)

val init_chance : string -> int -> string -> string -> int -> t
(** [init_chance name price command] is the initial state of a chance card. *)

val name : t -> string
(** [name c] is the name of chance [c].*)

val price : t -> int
(** [price c] is the price of chance [c]. *)

val command : t -> string
(** [command c] is the command of chance [c]. *)

val destination : t -> string
(** [destination c] is the destination of chance [c]. *)

val rel_space_translation : t -> int
(** [rel_space_translation c] is the number of spaces forward or backwards
    relative to the current location of chance [c]. *)

(* TODO: implement different player movements/actions based on commands*)
