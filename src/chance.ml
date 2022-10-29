type t = {
  name: string;
  price: int;
  command: string;
}

let init_chance description charge command= 
  {
    name =  description;
    price = charge;
    command = command;

  }

let name t = t.name 
let price t = t.price

let command t = t.command
