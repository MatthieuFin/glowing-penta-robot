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
    | RefType of glowyType
    | Top
    | Thrown
    | ExcptType
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
    | Tag of string * term * glowyType
    | Case of term * ((string * string * term) list)
    | Fix of term
    | Ref of term
    | Deref of term
    | Affect of term * term
    | Loc of int
    | Try of term * term
    | Raise of term
    | Error
;;

let type_exceptions = ref (VarType ([("exnPred0", Nat);("failure", UnitType)]));;
