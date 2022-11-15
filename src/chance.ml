type t = {
  name: string;
  price: int;
  command: string;
  destination: string;
}

let init_chance description charge command dest= 
  {
    name =  description;
    price = charge;
    command = command;
    destination = dest;

  }

let name t = t.name 
let price t = t.price

let command t = t.command

let destination t = t.destination


