type t = {
  name: string;
  destination: string;
  payment: int;

  (* Payment Source: Who is actually paying? The players or the bank?*)
  payment_source: string
}

let destination t = t.destination

let name t = t.name

let payment t = t.payment

let payment_source t = t.payment_source

let init_chest nm dest p ps = 
  {
    name = nm;
    destination = dest;
    payment = p;
    payment_source = ps;
  }