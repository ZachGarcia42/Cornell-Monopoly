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

let tileName = function
  | Property p -> Property.name p
  | Go -> "Go"
  | CommunityChest h -> "Community Chest"
  | IncomeTax -> "Income Tax"
  | Chance c -> Chance.name c
  | JustVisiting -> "Just Visiting"
  | FreeParking -> "Free Parking"
  | GoToJail -> "Go To Jail"
  | LuxuryTax -> "Luxury Tax"

let get_price = function
  | Property p -> Property.price p
  | IncomeTax -> -200
  | LuxuryTax -> -100
  | Chance c -> Chance.price c
  | CommunityChest h -> Chest.payment h 
  | _ -> 0
