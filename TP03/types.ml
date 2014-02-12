(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)

type var = char;;

type term =
  | Var of var
  | App of term * term
  | Lambda of var * term
;; 
