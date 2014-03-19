(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)

(* TODO faire un truc intelligent *)
(* est ce qu'on sépare les types au niveau du caml ou
   on gerera directement dans notre programme? *)

type glowyType =
    | Bool
    | Nat
    | AppType of glowyType * glowyType
;;

type optType =
    | Some_type of glowyType
    | No_type of string
;;

type term = 
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
