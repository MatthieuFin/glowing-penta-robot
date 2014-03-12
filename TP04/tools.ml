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

(* Remplace les occurence de x (un nom de variable formelle) 
   dans t (un terme) par y (un terme) *)
let rec substitute ty t1 t2 t = 
    match t with
      | Var x when x = t1 -> t2
      | App (t3, t4) -> App ((substitute t1 t2 t3), (substitute t1 t2 t4))
      | Lambda (x, t3) when x <> t1 -> Lambda (x, (substitute t1 t2 t3))
      | Pred
      | IsZero
      | Succ
      | Cond
      | _ -> t
;;

(* Donne la valeur d'une variable *)
let getValue alias = 
    try Hashtbl.find tblOfSymbols alias
    with Not_found -> Var alias
;;




