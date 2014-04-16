(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)
open Types ;;

let tblOfSymbols = Hashtbl.create 1;;

(* Affecte la valeur value a l'alias alias *)
let declare (alias : string) (value : term) = 
    Hashtbl.replace tblOfSymbols alias value;
    value
;;

(* Remplace les occurence de t1 (un nom de variable formelle) 
   dans t (un terme) par t2 (un terme) *)
let rec substitute ty t1 t2 t = 
    match t with
      | Var x when x = t1 -> t2
      | App (t3, t4) -> App ((substitute ty t1 t2 t3), (substitute ty t1 t2 t4))
      | Lambda (ty, x, t3) when x <> t1 -> Lambda (ty, x, (substitute ty t1 t2 t3))
      | Pred x -> Pred (substitute ty t1 t2 x)
      | IsZero x -> IsZero (substitute ty t1 t2 x)
      | Succ x -> Succ (substitute ty t1 t2 x)
      | Cond (bo, tr, fa) -> Cond ((substitute ty t1 t2 bo), (substitute ty t1 t2 tr), (substitute ty t1 t2 fa))
      | _-> t
;;

(* Donne la valeur d'une variable *)
let getValue alias = 
    try Some( Hashtbl.find tblOfSymbols alias)
    with Not_found -> None
;;




