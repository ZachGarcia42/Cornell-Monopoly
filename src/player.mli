type player
(** The abstract type of values representing a monopoly player. *)

val init_player : string -> int -> player
(** [init_player name pos amt] is a player initialized at Go with name [name],
    play order [pos], and [amt] cash*)

val name : player -> string
(** [name player] is the name of the player*)

val cards : player -> int
(** [cards player] is the number of get out of jail free cards the player has*)

val use_card : player -> player
(** [use_card player] represents the player after they used a get out of jail
    free card to leave jail.*)

val location : player -> int
(** [location player] is the index of the tile the player is on*)

val in_jail : player -> bool
(** [in_jail player] is true if the player is in jail and false otherwise*)

val go_to_jail : player -> player
(**[go_to_jail player] is an updated player now in jail*)

val turns_in_jail : player -> int
(**[turns_in_jail player] is the number of turns the player has been in jail*)

val jail_turn : player -> player
(**[jail_turn p] represents a player who has spent an additional turn in jail*)

val leave_jail : player -> player
(**[leave_jail player] represents a player who has left jail*)

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

val purchase_property : player -> Tile.tile -> player
(** Purchases the property and updates the player's values*)

val buy_property : player -> Property.t -> player
(** [buy_property player prop roll] is an updated player with prop added to the
    property list and the cost of prop subtracted from their cash*)

val sell_property : player -> Property.t -> player
(** [buy_property player prop roll] is an updated player with prop subtracted
    from the property list and half the cost of prop added from their cash*)

val net_worth : player -> int
(** [net_worth player] is the cash value plus property value of this player*)

val add_get_out_card : player -> player
(* [add_get_out_card player] adds another get_out card*)

val handle_chance : player -> Chance.t -> Tile.tile -> int -> int -> player
(* [handle_chance player ch property] updates the player's status based on the
   chance card*)

val unlock_chance_card : player -> Tile.tile -> int -> int -> player
(* [unlock_chance_card player property old_position new_position] picks a random
   chance card and calls handle chance*)

val handle_cc : player -> player list -> Chest.t -> Tile.tile -> player list
(* [handle_cc player playerlst ch property] updates the player's status based on
   the community chest card*)

val unlock_comm_chest_card : player -> Tile.tile -> player list -> player list
(* [unlock_comm_card player property] unlocks a community chest card and updates
   the player's status*)

val pay_tax : player -> Tile.tile -> player
(* [unlock_comm_card player property] pays a tax for a player*)

val string_list_properties : player -> string
(**[string_list_properties player] converts the list of player properties into a
   string*)

val player_name_to_property : player -> string -> Property.t option
(*[player_name player str] matches a string [str] to a property that player
  [player] has purchased. Returns None if [str] is not the name of a player's
  property.*)
