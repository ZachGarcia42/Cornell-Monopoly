val board : Tile.t list
(** The game board containing all monopoly squares in the current game*)

val square_landed : Tile.t list -> int -> int -> string
(* Returns the name of the particular square of the board that the player landed
   on depending on their dice roll and their initial position*)

val parse_user_input: string -> string 
(* Parses a string, removing white space and converting 
   all inputs to strings with capital letters*)
