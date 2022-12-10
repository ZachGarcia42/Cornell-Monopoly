type t
(** An abstract type t for a community chest card*)

val destination : t -> string
(** [destination c] is the destination for a certain community chest card, [c]*)

val name : t -> string
(** [name c] is the name for a certain community chest card, [c]*)

val payment : t -> int
(** [payment c] is the payment amount for a certain community chest card, [c]*)

val payment_source : t -> string
(** [payment_source c] is the payment source for a [c]*)

val init_chest : string -> string -> int -> string -> t
(** [init_chest name destination payment payment_source] is the initial state of
    a community chest card. *)
