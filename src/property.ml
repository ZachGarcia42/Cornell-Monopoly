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
let price t = t.price
