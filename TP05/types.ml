(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)

type glowyType =
    | UnitType
    | Bool
    | Nat
    | AppType of glowyType * glowyType
    | RcdType of (string * glowyType) list
    | VarType of (string * glowyType) list
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
    | Name of string * term * term
    | Record of (string * term) list
    | Projection of (term * string)
    | Variant of (string * term) list
;;
