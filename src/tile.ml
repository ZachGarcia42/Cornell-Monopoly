(** The type representing a single monopoly board tile*)
type tile =
  | Property of Property.t
  | Go
  | CommunityChest of Chest.t
  | IncomeTax
  | Chance of Chance.t
  | JustVisiting
  | FreeParking
  | GoToJail
  | LuxuryTax

(** [tile_name t] is the name of [t]*)
let tile_name = function
  | Property p -> Property.name p
  | Go -> "Go"
  | CommunityChest h -> "Community Chest"
  | IncomeTax -> "Income Tax"
  | Chance c -> "Chance"
  | JustVisiting -> "Just Visiting"
  | FreeParking -> "Free Parking"
  | GoToJail -> "Go To Jail"
  | LuxuryTax -> "Luxury Tax"

(** [get_price t] is the price of [t]*)
let get_price = function
  | Property p -> Property.price p
  | IncomeTax -> -200
  | LuxuryTax -> -100
  | Chance c -> Chance.price c
  | CommunityChest h -> Chest.payment h
  | _ -> 0

(** [get_pos board dest acc] is the index of the tile with name [dest] in
    [board] *)
let rec get_pos board dest acc =
  match board with
  | [] -> acc
  | h :: t ->
      let sq = tile_name h in
      if sq = dest then acc else get_pos t dest (acc + 1)
