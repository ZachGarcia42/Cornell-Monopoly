type player
(** The abstract type of values representing a monopoly player. *)

val init_player : string -> int -> player
(** [init_player name pos amt] is a player initialized at Go with name [name],
    play order [pos], and [amt] cash*)

val name : player -> string
(** [name player] is the name of the player*)

val cards : player -> int
(** [cards player] is the number of get out of jail free cards the player has*)

val location : player -> int
(** [location player] is the index of the tile the player is on*)

val in_jail : player -> bool
(** [in_jail player] is true if the player is in jail and false otherwise*)

val go_to_jail : player -> player
(**[go_to_jail player] is an updated player now in jail*)

val move_to : player -> int -> player
(** [move player ind] is an updated player with position [ind]. Requires ind is
    non-negative and at most the maximum index of the tile list*)

val cash : player -> int
(** [cash player] is the amount of cash the player has*)

val pay : player -> int -> player
(** [pay player amt] is an updated player with cash now more than before by amt*)

val charge : player -> int -> player
(** [charge player amt] is an updated player with cash now less than before by
    amt*)

val properties : player -> Property.t list
(** [properties player] is the list of properties the player owns*)

val has_property : player -> Property.t -> bool
(** [has_property player prop] is true if prop is in the player's properties
    list and false otherwise*)

val buy_property : player -> Property.t -> player
(** [buy_property player prop roll] is an updated player with prop added to the
    property list and the cost of prop subtracted from their cash*)

val net_worth : player -> int
(** [net_worth player] is the cash value plus property value of this player*)

val add_get_out_card: player -> player 
(* [add_get_out_card player] adds another get_out card*)
