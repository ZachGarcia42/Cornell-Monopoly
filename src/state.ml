open Player
open Tile
open Board
open Chance
open Property
open Chest
open Printer

let starting_money = 1500

type state = {
  players : player list;
  purchased_properties : int list;
  money_jar : int;
}

let player_list state = state.players
let purchased_properties state = state.purchased_properties
let money_jar state = state.money_jar

let init_state (players : player list) props money : state =
  { players; purchased_properties = props; money_jar = money }

let check_properties state location =
  match List.nth board location with
  | Property x -> List.mem location state.purchased_properties
  | _ -> false
(*List.mem location purchased *)

(** [add_properties purchased location] adds the property at index [location] to
    the list of purchased properties. *)
let add_properties purchased location = location :: purchased

(**[remove_properties purchased property] removes the property [property] from
   the list of purchased properties*)
let remove_properties state property =
  init_state (player_list state)
    (List.filter
       (fun p -> if p = property then false else true)
       (purchased_properties state))
    (money_jar state)

(** [check_player_properties prop properties] is true iff one of the properties
    matches [prop]. *)
let rec check_player_properties prop = function
  | [] -> false
  | property :: t -> property = prop || check_player_properties prop t

(** [is_property_owned property] is true iff [property] is owned by one of the
    players. *)
let rec is_property_owned (property : Property.t) players =
  match players with
  | [] -> false
  | player :: t ->
      check_player_properties property (properties player)
      || is_property_owned property t

(** [find_owner p players] is the name of the owner of property [p] from the
    [players] list *)
let rec find_owner (p : Property.t) (players : Player.player list) =
  match players with
  | [] -> "No one"
  | h :: t -> if Player.has_property h p then Player.name h else find_owner p t
