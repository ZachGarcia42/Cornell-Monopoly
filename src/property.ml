type color =
  | Red
  | Yellow
  | Green
  | Blue
  | Brown
  | LightBlue
  | Magenta
  | Orange

type t = {
  name : string;
  color : color;
  price : int;
}

let init_property name color price = { name; color; price }
let name t = t.name
let color t = t.color
let price t = t.price
