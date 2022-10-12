
type board 
(* Our abstract type which defines a Monopoly Board*)

val is_player_turn: board -> bool 
(* Returns true or false depending on whether it is a particular 
   player's turn*)

val square_landed: board -> int -> int -> string
(* Returns the name of the particular square of the board that the player landed on 
   depending on their dice roll and their initial position*)