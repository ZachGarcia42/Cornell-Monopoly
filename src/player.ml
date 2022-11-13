type player = {
  name : string;
  get_out_cards : int;
  tile : int;
  properties : Property.t list;
  cash : int;
  in_jail : bool;
}

let init_player name amt =
  {
    name;
    get_out_cards = 0;
    tile = 0;
    properties = [];
    cash = amt;
    in_jail = false;
  }

let name player = player.name
let cards player = player.get_out_cards
let location player = player.tile
let in_jail player = player.in_jail
let go_to_jail player = { player with in_jail = true }
let move_to player ind = { player with tile = ind }
let cash player = player.cash
let pay player amt = { player with cash = player.cash + amt }
let charge player amt = { player with cash = player.cash - amt }
let properties player = player.properties
let has_property player prop = List.mem prop player.properties

let add_get_out_card player = {player with get_out_cards = player.get_out_cards + 1}

let buy_property player prop =
  {
    player with
    cash = player.cash - Property.price prop;
    properties = prop :: player.properties;
  }

let net_worth p =
  p.cash
  + List.fold_left (fun sum prop -> sum + Property.price prop) 0 p.properties
