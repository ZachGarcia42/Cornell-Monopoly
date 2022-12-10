type state
(** The abstract type of values representing a single monopoly game state. *)

val starting_money : int
(** The default starting money for each player. *)

val player_list : state -> Player.player list
(** [player_list state] returns the list of players currently active in the
    game.*)

val purchased_properties : state -> int list
(** [purchased_properties s] is [s.purchased_properties]*)

val init_state : Player.player list -> int list -> state
(** [init_state players props money] is the state containing [players], [props],
    and money*)

val check_properties : state -> int -> bool
(** [check_properties state location] returns true iff the property at index
    [location] of the game board has been purchased. False otherwise. *)

val add_properties : int list -> int -> int list
(** [add_properties purchased location] adds the property at index [location] to
    the list of purchased properties. *)

val remove_properties : state -> int -> state
(** [remove_properties purchased location] removes the property at index
    [location] from the list of purchased properties*)

val is_property_owned : Property.t -> Player.player list -> bool
(** [is_property_owned property] is true iff [property] is owned by one of the
    players. *)

val find_owner : Property.t -> Player.player list -> string
(** [find_owner p players] is the name of the owner of property [p] from the
    [players] list *)