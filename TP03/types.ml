(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)

type term =
  | Var of string
  | App of term * term
  | Lambda of string * term
;;
