type t
(** The abstract type of values representing a single monopoly property. *)

(** The type representing the color of a property. *)
type color =
  | Red
  | Yellow
  | Green
  | Blue
  | Brown
  | LightBlue
  | Magenta
  | Orange

val init_property : string -> color -> int -> t
(** [init_property name color price] is the initial state of a property. *)

val name : t -> string
(** [name p] is the name of property [p].*)

val color : t -> color
(** [color p] is the color of property [p]. *)

val price : t -> int
(** [price p] is the price of property [p]. *)
