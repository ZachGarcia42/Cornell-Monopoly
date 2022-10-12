open Monopoly

type player 
(* An abstract type player that we define*)

val money_left : player -> int 
(* Gets the amount of money that a player has*)

val has_enough_money: player -> board -> int -> bool 
(* Returns whether a player can spend money or not*)

val is_bankrupt: int -> bool 
(* Returns whether a player is bankrupt or not *)

