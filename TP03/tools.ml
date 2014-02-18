(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)
open Types ;;

let tblOfSymbols = Hashtbl.create 1;;

(*TODO Affecte la valeur value a l'alias alias *)
let declare (alias : string) (value : term) = 
    Hashtbl.replace tblOfSymbols alias value;
    value
;;

(* Remplace les occurence de x (un nom de variable formel) 
   dans t (un terme) par y (un terme) *)
let rec substitute x y t =
    match t with
      | Var z when z = x -> y
      | Var z -> Var z
      | App (i, j) -> App ((substitute x y i), (substitute x y j))
      | _ -> Var "en-cours"
;;

(* Donne la valeur d'une variable *)
let getValue alias = 
    try Hashtbl.find tblOfSymbols alias
    with Not_found -> Var alias
;;


