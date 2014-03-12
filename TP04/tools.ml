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
let rec substitute t1 t2 t = t
;;

(* Donne la valeur d'une variable *)
let getValue alias = 
    try Hashtbl.find tblOfSymbols alias
    with Not_found -> Var alias
;;




