open Yojson.Basic.Util
open Chance
open Chest

(** [draw_card cards] is a random element in [cards] *)
let draw_card cards =
  let idx = Random.int (List.length cards) in
  List.nth cards idx

(** [data_dir_prefix] is the path to the data directory *)
let data_dir_prefix = "data" ^ Filename.dir_sep

(** [tiles] is a Yojson representation of the game board *)
let tiles = Yojson.Basic.from_file (data_dir_prefix ^ "board.json")

(** [chance_cards] is a Yojson representation of the Chance cards*)
let chance_cards = Yojson.Basic.from_file (data_dir_prefix ^ "chance.json")

(** [chest_cards] is a Yojson representation of the Community Chest cards*)
let chest_cards = Yojson.Basic.from_file (data_dir_prefix ^ "chest.json")

(** [retr_list j s f] is an OCaml list that represents the elements stored in
    [s] of the Yojson structure, [j], after [f] has been applied to them *)
let retr_list j s f = j |> member s |> to_list |> List.map f

(** [yojson_string j s] is a string representation of the data stored at [s] in
    [j] *)
let yojson_string j s = j |> member s |> to_string

(** [yojson_int j s] is an integer representation of the data stored at [s] in
    [j] *)
let yojson_int j s = j |> member s |> to_int

(** [to_chance j] is the single [Chance.t] card represented by [j]*)
let to_chance j =
  init_chance (yojson_string j "name") (yojson_int j "price")
    (yojson_string j "command")
    (yojson_string j "tile") (yojson_int j "movement")

(** [chance_commands] is the list of all [Chance.t] cards*)
let chance_commands = retr_list chance_cards "cards" to_chance

(** [to_chest j] is the single [Chest.t] card represented by [j]*)
let to_chest j =
  init_chest (yojson_string j "name") (yojson_string j "tile")
    (yojson_int j "price") (yojson_string j "source")

(** [community_chest_commands] is the list of all [Chest.t] cards*)
let community_chest_commands = retr_list chest_cards "cards" to_chest

(** [to_prop j] is the single [Tile.tile] represented by [j]*)
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

(** [board] is the list of all [Tile.tile] tiles on the board*)
let board : Tile.tile list = retr_list tiles "tiles" to_prop