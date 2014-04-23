(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)
open Types ;;

exception Field_Not_Found of string;;

let tblOfSymbols = Hashtbl.create 1;;

(* Affecte la valeur value a l'alias alias *)
let declare (alias : string) (value : term) = 
    Hashtbl.replace tblOfSymbols alias value;
    value
;;

(* Remplace les occurence de t1 (un nom de variable formelle) 
   dans t (un terme) par t2 (un terme) *)
let rec substitute t1 t2 t = 
    match t with
      | Var x when x = t1 -> t2
      | App (t3, t4) -> App ((substitute  t1 t2 t3), (substitute  t1 t2 t4))
      | Lambda (ty, x, t3) when x <> t1 -> Lambda (ty, x, (substitute  t1 t2 t3))
      | Pred x -> Pred (substitute  t1 t2 x)
      | IsZero x -> IsZero (substitute  t1 t2 x)
      | Succ x -> Succ (substitute  t1 t2 x)
      | Cond (bo, tr, fa) -> Cond ((substitute  t1 t2 bo), (substitute  t1 t2 tr), (substitute t1 t2 fa))
      | _-> t
;;

(* Donne la valeur d'une variable *)
let getValue alias = 
    try Some( Hashtbl.find tblOfSymbols alias)
    with Not_found -> None
;;

let get_var_name term =
    let rec aux term =
        match term with
          | Var x -> x
          | App (t3, t4) -> "\'" ^ (aux t3) ^ (aux t4)
          | Lambda (typ, x, t) -> "\'" ^ x ^ (aux t)
          | Cond (bo, tr, fa) -> "\'" ^ (aux bo) ^ (aux tr) ^ (aux fa)
          | Pred x -> "\'" ^ (aux x)
          | Succ x -> "\'" ^ (aux x)
          | IsZero x -> "\'" ^ (aux x)
          | Unit -> "\'u"
          | True -> "\'t"
          | False -> "\'f"
          | Zero -> "\'0"
          | Name (alias, t1, t2) -> "\'" ^ alias ^ (aux t1) ^ (aux t2)
          | Record _ -> "\'r"
          | Projection _ -> "\'p"
    in
    "\'" ^ (aux term)
;;

let rec find_field liste label =
    match liste with
      | [] -> raise (Field_Not_Found label)
      | (tag, value)::l' when tag = label -> value
      | _::l' -> find_field l' label
;;

let rec elaborate terme =
    match terme with
      | Lambda (typ, var, term) -> Lambda (typ, var, elaborate term)
      | App (t1, t2) -> App (elaborate t1, elaborate t2)
      | _ -> terme
;;




