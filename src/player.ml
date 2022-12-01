open Property
open Printer
open Tile
open Board
open Chance
open Monopoly

type player = {
  name : string;
  get_out_cards : int;
  tile : int;
  properties : Property.t list;
  cash : int;
  in_jail : bool;
  turns_in_jail : int;
}

let init_player name amt =
  {
    name;
    get_out_cards = 0;
    tile = 0;
    properties = [];
    cash = amt;
    in_jail = false;
    turns_in_jail = 0;
  }

let name player = player.name
let cards player = player.get_out_cards
let location player = player.tile
let in_jail player = player.in_jail
let go_to_jail player = { player with in_jail = true }
let turns_in_jail player = player.turns_in_jail
let leave_jail player = { player with in_jail = false; turns_in_jail = 0 }
let jail_turn player = { player with turns_in_jail = player.turns_in_jail + 1 }

let use_card player =
  leave_jail { player with get_out_cards = player.get_out_cards - 1 }

let move_to player ind = { player with tile = ind }
let cash player = player.cash
let pay player amt = { player with cash = player.cash + amt }
let charge player amt = { player with cash = player.cash - amt }
let properties player = player.properties
let has_property player prop = List.mem prop player.properties

let add_get_out_card player =
  { player with get_out_cards = player.get_out_cards + 1 }

let buy_property player prop =
  {
    player with
    cash = player.cash - Property.price prop;
    properties = prop :: player.properties;
  }

(** [purchase_property player property] is the updated [player] after they have
    purchased [property]. Requires: [property] is a tile, but all uses of this
    function will have it be a tile property. Other match cases are for code
    safety. *)
let purchase_property (player : player) (property : Tile.tile) =
  match property with
  | Go ->
      print_typed_string "Collect $200 here! You can't purchase this";
      charge player 0
  | Property x ->
      print_typed_string
        "Congratulations! You have successfully purchased this property.";
      buy_property player x
  | IncomeTax ->
      print_typed_string
        "Sorry, this is a tax! You cannot purchase \n\
        \    this property! Deducting 200 to pay the tax! ";
      charge player 200
  | LuxuryTax ->
      print_typed_string
        "Sorry, this is a tax! You cannot purchase \n\
        \    this property! Deducting 100 to pay the tax! ";
      charge player 100
  | _ -> charge player 0

(** [charged_player player property] is the updated player after they've been
    charged for landing on [property]*)
let charged_player (player : player) (property : Property.t) =
  charge player (Property.price property)

let sell_property player prop =
  {
    player with
    cash = player.cash + (Property.price prop / 2);
    properties =
      List.filter
        (fun property -> if property = prop then false else true)
        player.properties;
  }

let net_worth p =
  p.cash
  + List.fold_left (fun sum prop -> sum + Property.price prop) 0 p.properties

let unlock_chance_card (player : player) property =
  match property with
  | Chance c ->
      let dest = Chance.destination c in
      let ctype = Chance.name c in
      let price = Chance.price c in
      if ctype = "Chance: Advancement" then (
        print_typed_string ("You have advanced to " ^ dest);
        let new_pos = get_pos board dest 0 in
        let did_player_pass_go =
          if
            Monopoly.player_passed_go
              (get_pos board (tileName property) 0)
              (get_pos board dest 0)
          then pay player 200
          else player
        in
        move_to did_player_pass_go new_pos)
      else if ctype = "Chance: Money Made" then pay player price
      else if ctype = "Chance: Payment Required" then charge player price
      else if ctype = "Chance: Move Backwards" then (
        let current_pos = get_pos board (tileName property) 0 in

        let dest =
          Monopoly.convert
            (current_pos + rel_space_translation c)
            (List.length board)
        in
        print_endline
          ("You are being moved back to " ^ tileName (List.nth board dest));

        let current_tile = List.nth board dest in

        let new_player =
          match current_tile with
          | IncomeTax -> 
            print_endline "You are being charged $200";
            charge player 200
          | LuxuryTax -> 
            print_endline "You are being charged $100";
            charge player 100
          | FreeParking -> 
            print_endline "You receive $100";
            pay player 100
          | _ -> player
        in

        move_to new_player dest)
      else if ctype = "Chance: Get out of Jail Free" then (
        let num_get_out_of_jail_free_cards = player.get_out_cards in
        print_endline "You have earned a Get out of Jail Free Card! ";
        print_endline
          ("You now have "
          ^ string_of_int (num_get_out_of_jail_free_cards + 1)
          ^ " Get out of Jail Free Cards!");
        add_get_out_card player)
      else player
  | _ ->
      print_typed_string "This is not a Chance Card!";
      charge player 0

let unlock_comm_chest_card (player : player) property =
  match property with
  | CommunityChest c ->
      print_endline (Chest.name c);
      if Chest.name c = "Get out of Jail Free Card earned" then
        { player with get_out_cards = player.get_out_cards + 1 }
      else
        let dest = Chest.destination c in
        if dest <> "Current" then (
          let new_pos = get_pos board dest 0 in
          print_typed_string ("You have advanced to " ^ dest);

          let did_player_pass_go =
            if
              Monopoly.player_passed_go
                (get_pos board (tileName property) 0)
                (get_pos board dest 0)
            then pay player 200
            else player
          in

          move_to did_player_pass_go new_pos)
        else
          let payment = Chest.payment c in
          if payment < 0 then charge player (-1 * payment)
          else pay player payment
  | _ ->
      print_endline "This is not a Community Chest Card!";
      player

let pay_tax (player : player) property =
  match property with
  | IncomeTax -> charge player 200
  | LuxuryTax -> charge player 100
  | _ ->
      print_endline "You don't have to pay any tax :) !";
      charge player 0

let string_list_properties player =
  List.fold_left
    (fun (acc : string) (h : Property.t) -> acc ^ " | " ^ Property.name h)
    "" (properties player)

let rec player_name_to_property (player : player) str =
  match properties player with
  | [] -> None
  | h :: t ->
      if Monopoly.parse_user_input (Property.name h) = str then Some h
      else player_name_to_property (sell_property player h) str

let state_sell_prop (player : player) property = sell_property player property
