(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)
open Types ;;

exception Field_Not_Found of string;;
exception Unbound_Variable of string;;
exception Bad_Type of string;;
exception Unbound_Alias of string;;
exception Unbound_Location of int;;
exception Bad_Tag_Type of string * glowyType * glowyType;;
exception Var_Tag_Not_Found of string;;
exception Not_Record_Throw of term * string;;
exception Cant_Meet of glowyType * glowyType;;

let tblOfSymbols = Hashtbl.create 1;;
let mu = Hashtbl.create 1;;
let sigma = Hashtbl.create 1;;

(* Affecte la valeur value a l'alias alias *)
let declare (alias : string) (value : term) = 
    Hashtbl.replace tblOfSymbols alias value;
    value
;;

(* enregistre un terme dans la mémoire *)
let setRef (loc : int) (terme : term) (typ : glowyType) =
    Hashtbl.replace mu loc terme;
    Hashtbl.replace sigma loc typ;
    Loc loc
;;

let isSetRef (loc : int) =
    Hashtbl.mem mu loc
;;

let getRefValue (loc : int) =
    try Hashtbl.find mu loc
    with Not_found -> raise (Unbound_Location loc)
;;
(* Remplace les occurence de t1 (un nom de variable formelle) 
   dans t (un terme) par t2 (un terme) *)
let rec substitute t1 t2 t = 
    match t with
      | Unit | True | False | Zero -> t
      | Var x when x = t1 -> t2
      | Var x -> t
      | App (t3, t4) -> App ((substitute  t1 t2 t3), (substitute  t1 t2 t4))
      | Lambda (ty, x, t3) when x <> t1 -> Lambda (ty, x, (substitute  t1 t2 t3))
      | Lambda _ -> t
      | Pred x -> Pred (substitute  t1 t2 x)
      | IsZero x -> IsZero (substitute  t1 t2 x)
      | Succ x -> Succ (substitute  t1 t2 x)
      | Cond (bo, tr, fa) -> Cond ((substitute  t1 t2 bo), (substitute  t1 t2 tr), (substitute t1 t2 fa))
      | Name (label, value, terme) when label <> t1-> Name (label, (substitute t1 t2 value), (substitute t1 t2 terme))
      | Name (label, value, terme) -> Name (label, (substitute t1 t2 value), terme)
      | Record l -> Record (substitute_in_record t1 t2 l)
      | Projection (terme, label) -> Projection (substitute t1 t2 terme, label)
      | Tag (label, value, typ) -> Tag (label, (substitute t1 t2 value), typ)
      | Case (term, l) -> Case (substitute t1 t2 term, substitute_cases t1 t2 l)
      | Fix terme -> Fix (substitute t1 t2 terme)
      | Ref terme -> Ref (substitute t1 t2 terme)
      | Deref terme -> Deref (substitute t1 t2 terme)
      | Affect (term1, term2) -> Affect((substitute t1 t2 term1),(substitute t1 t2 term2))
      | Loc label -> t (*TODO a verifier *)
and substitute_in_record t1 t2 l =
    match l with
      | [] -> []
      | (label, term)::l' -> (label,substitute t1 t2 term)::(substitute_in_record t1 t2 l')
and substitute_cases t1 t2 l =
    match l with
      | [] -> []
      | (label, alias, value)::l' ->
            (label, alias, substitute t1 t2 value)::(substitute_cases t1 t2 l')
;;

(* Donne la valeur d'une variable *)
let getValue alias = 
    try Hashtbl.find tblOfSymbols alias
    with Not_found -> raise (Unbound_Variable alias)
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
          | Tag _ -> "\'t"
          | Projection _ -> "\'p"
          | Case _ -> "\'c"
          | _ -> "\'"
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




