type t 
(* An abstract type t for a community chest card*)

val destination : t -> string 
(* A destination for a certain community chest card*)

val name: t -> string 
(* A name for a certain community chest card*)

val payment: t -> int 
(* A payment for a certain community chest card*)

val payment_source: t -> string 
(* A payment source for a certain community chest card*)