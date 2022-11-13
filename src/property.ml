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
  index : int;
}

let init_property name set price index = { name; set; price; index }
let name t = t.name
let color t = t.set
let price t = t.price
let index t = t.index
