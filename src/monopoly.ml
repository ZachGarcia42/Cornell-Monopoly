let board = Board.board

(* Accounts for the fact that positions may need to wrap around to the beginning
   of the board again*)
let rec convert (new_pos : int) (square_list_length : int) =
  if new_pos >= 0 && new_pos < square_list_length then new_pos
  else if new_pos >= square_list_length then
    convert (new_pos - square_list_length) square_list_length
  else convert (new_pos + square_list_length) square_list_length


(* init_pos is the initial position in the board where they are at. fin_pos is the 
   roll on the dice that they got*)
let square_landed board init_pos dice_roll =
  let new_pos = convert (init_pos + dice_roll) (List.length board) in
  let sq = List.nth board new_pos in
  Tile.tileName sq
