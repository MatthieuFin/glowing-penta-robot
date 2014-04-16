(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)

(* TODO faire un truc intelligent *)
(* est ce qu'on sépare les types au niveau du caml ou
   on gerera directement dans notre programme? *)

type glowyType =
    | UnitType
    | Bool
    | Nat
    | AppType of glowyType * glowyType
;;

type term = 
    | Unit
    | True
    | False
    | Zero
    | Cond of term * term * term
    | Succ of term 
    | Pred of term
    | IsZero of term
    | Var of string
    | App of term * term
    | Lambda of glowyType * string * term
;;
