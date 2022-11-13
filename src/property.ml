type set =
  | Red
  | Yellow
  | Green
  | Blue
  | Brown
  | LightBlue
  | Magenta
  | Orange
  | Railroad
  | Utility

type t = {
  name : string;
  set : set;
  price : int;
}

let init_property name set price = { name; set; price }
let name t = t.name
let color t = t.set

let string_of_set = function
  | Red -> "Red"
  | Yellow -> "Yellow"
  | Green -> "Green"
  | Blue -> "Blue"
  | Brown -> "Brown"
  | LightBlue -> "Light Blue"
  | Magenta -> "Magenta"
  | Orange -> "Orange"
  | Railroad -> "Railroad"
  | Utility -> "Utility"

let price t = t.price
