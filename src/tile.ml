type t =
  | Property of Property.t
  | Go
  | CommunityChest
  | IncomeTax
  | Chance
  | JustVisiting
  | FreeParking
  | GoToJail
  | LuxuryTax

let tileName = function
  | Property p -> Property.name p
  | Go -> "Go"
  | CommunityChest -> "Community Chest"
  | IncomeTax -> "Income Tax"
  | Chance -> "Chance"
  | JustVisiting -> "Just Visiting"
  | FreeParking -> "Free Parking"
  | GoToJail -> "Go To Jail"
  | LuxuryTax -> "Luxury Tax"