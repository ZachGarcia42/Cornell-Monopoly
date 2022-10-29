type t 
(* The abstract type representing a Chance Card*)

val init_chance : string -> int -> string -> t
(** [init_chance name price command] is the initial state of a chance card. *)

val name : t -> string
(** [name c] is the name of chance [c].*)

val price : t -> int
(** [price c] is the price of chance [c]. *)

val command : t -> string 
(** [command c] is the command of chance [c]. *)

(* TODO: implement different player movements/actions based on commands*)
