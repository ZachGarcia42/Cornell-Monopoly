open Yojson.Basic.Util
open Chance
open Chest

let draw_card cards =
  let idx = List.length cards - 2 (* Random.int (List.length cards) *)in
  List.nth cards idx

let data_dir_prefix = "data" ^ Filename.dir_sep
let tiles = Yojson.Basic.from_file (data_dir_prefix ^ "board.json")
let chance_cards = Yojson.Basic.from_file (data_dir_prefix ^ "chance.json")
let chest_cards = Yojson.Basic.from_file (data_dir_prefix ^ "chest.json")
let retr_list j s f = j |> member s |> to_list |> List.map f
let yojson_string j s = j |> member s |> to_string
let yojson_int j s = j |> member s |> to_int

let to_chance j =
  init_chance (yojson_string j "name") (yojson_int j "price")
    (yojson_string j "command")
    (yojson_string j "tile") (yojson_int j "movement")

let chance_commands = retr_list chance_cards "cards" to_chance

let to_chest j =
  init_chest (yojson_string j "name") (yojson_string j "tile")
    (yojson_int j "price") (yojson_string j "source")

let community_chest_commands = retr_list chest_cards "cards" to_chest

let to_prop j : Tile.tile =
  match yojson_string j "type" with
  | "Go" -> Go
  | "IncomeTax" -> IncomeTax
  | "LuxuryTax" -> LuxuryTax
  | "GoToJail" -> GoToJail
  | "JustVisiting" -> JustVisiting
  | "FreeParking" -> FreeParking
  | "CommunityChest" -> CommunityChest (draw_card community_chest_commands)
  | "Chance" -> Chance (draw_card chance_commands)
  | "Property" ->
      Property
        (Property.init_property (yojson_string j "name")
           (Property.set_of_string (yojson_string j "color"))
           (yojson_int j "price") (yojson_int j "pos"))
  | s -> failwith ("Parsing error" ^ s)

let board : Tile.tile list = retr_list tiles "tiles" to_prop

let chance_names : string list =
  [
    "Chance: Advancement";
    "Chance: Payment Required";
    "Chance: Money Made";
    "Chance: Get out of Jail Free";
  ]

(*TODO: Add chance commands for the chance names above. Then randomly pick a
  chance name and command correspondingly for the above board list*)