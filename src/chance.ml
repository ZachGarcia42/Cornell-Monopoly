type t = {
  name: string;
  price: int;
  command: string;
  destination: string;
  rel_sp_translation: int;
}

let init_chance description charge command dest sp= 
  {
    name =  description;
    price = charge;
    command = command;
    destination = dest;
    rel_sp_translation = sp;
  }

let name t = t.name 
let price t = t.price

let command t = t.command

let destination t = t.destination

let rel_space_translation t = t.rel_sp_translation


