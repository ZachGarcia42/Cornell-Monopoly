type t
(** The abstract type of values representing a single monopoly property. *)

(** The type representing the set of a property. *)
type set =
  | Red
  | Yellow
  | Green
  | Blue
  | Brown
  | LightBlue
  | Magenta
  | Orange
  | Railroad
  | Utility

val init_property : string -> set -> int -> t
(** [init_property name color price] is the initial state of a property. *)

val name : t -> string
(** [name p] is the name of property [p].*)

val color : t -> set
(** [color p] is the color of property [p]. *)

val string_of_set : set -> string
(** [string_of_set set] is the same [set] but as a string. *)

val price : t -> int
(** [price p] is the price of property [p]. *)
