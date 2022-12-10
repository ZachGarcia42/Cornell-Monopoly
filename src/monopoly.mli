val board : Tile.tile list
(** The game board containing all monopoly squares in the current game*)

val square_landed : Tile.tile list -> int -> int -> string
(** [square_landed board init_pos dice_roll] is the name of the particular
    square of the board that the player landed on depending on their dice roll,
    [dice_roll], and their initial position, [init_pos]*)

val parse_user_input : string -> string
(** [parse_user_input s] parses a string, [s], removing white space and
    converting all inputs to strings with capital letters*)

val player_passed_go : int -> int -> bool
(** [player_passed_go init_pos fin_pos] is true if [fin_pos] is less than
    [init_pos] which indicates the player passed go. Return false if the player
    was initially on GoToJail*)

val convert : int -> int -> int
(** [convert new_pos board_len] converts an input, [new_pos] to be less than
    [board_len] with wraparound for numbers too large.*)
