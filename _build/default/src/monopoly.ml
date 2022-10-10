type property = {
  name: string ;
  price: int;
}

type square = {
  property: property;
  name: string;
}

type board = {
  squares: square list;
  is_player_turn: bool;
}

let is_player_turn (board) = board.is_player_turn








