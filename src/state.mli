type state
(** The abstract type of values representing a single monopoly game state. *)

val starting_money : int
(** The default starting money for each player. *)

val player_list : state -> Player.player list
(** [player_list state] returns the list of players currently active in the
    game.*)

val init_state : Player.player list -> state
(** [init_state players] is the state containing [players]*)

val inform_player : state -> Player.player -> Tile.tile -> int -> unit
(** [inform_player state player current_tile roll] prints a terminal output that
    informs players of essential information they need to begin each turn,
    including how much money they have and the property they are currently at. *)

val init_players : Player.player list -> Player.player list
(** [init_players ()] instantiates the number of desired players and adds them
    to the players list *)

val purchase_property : Player.player -> Tile.tile -> Player.player
(** Purchases the property and updates the player's values*)

val one_turn : state -> Player.player -> Player.player * int list * int
(** [one_turn player] represents a single turn for [player]. Returns the updated
    player record after turn has been completed. *)

val take_turns : state -> state
(** [take_turns players] represents the new player states after each player
    takes one turn*)
