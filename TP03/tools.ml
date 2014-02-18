(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)
open Types ;;

exception UnboundValue;;

let tblOfSymbols = Hashtbl.create 1;;

(*TODO Affecte la valeur value a l'alias alias *)
let declare (alias : string) (value : term) = 
    Hashtbl.replace tblOfSymbols alias value;
    value
;;

(* Remplace les occurence d'une variable par sa valeur *)
let rec substitute x y t =
  Var "x"
;;

(* Donne la valeur d'une variable *)
let getValue alias = 
    try Hashtbl.find tblOfSymbols alias
    with _ -> Var alias
;;


