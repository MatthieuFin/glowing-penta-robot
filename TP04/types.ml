(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)

(* TODO faire un truc intelligent *)
(* est ce qu'on sépare les types au niveau du caml ou
   on gerera directement dans notre programme? *)
type termite =
  | True
  | False
  | Zero
  | Cond of termite * termite * termite
  | Succ of termite 
  | Pred of termite
  | IsZero of termite
;;

type terminator =
  | Var of string
  | App of terminator * terminator
  | Lambda of string * terminator
;;

type terminal =
    | terminator
    | termite
;;

type term = 
    | Var of string
    | App of terminal * terminal
    | Lambda of string * terminal
;;
