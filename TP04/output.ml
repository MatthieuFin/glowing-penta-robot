open Types;;
open Eval;;
open TypeChecker;;

let getIndent i =
  let rec aux i res =
    match i with
      | 0 -> res
      | _ -> aux (i - 1) res ^ "    "
  in
    aux i ""
;;



(*TODO revoir affichage des termes pour faire un truc joli*)
let rec term_to_string term =
    match term with
      | True -> "True"
      | False -> "False"
      | Zero -> "0"
      | Cond (c, t, f) -> "If (" ^ (term_to_string c) ^ ") Then"
                          ^ (term_to_string t) 
                          ^ "Else" 
                          ^ (term_to_string f)
      | Succ (n) -> "Succ " ^ (term_to_string n)
      | Pred (n) -> "Pred " ^ (term_to_string n)
      | IsZero (n) -> "IsZero " ^ (term_to_string n)
      | Var (s) -> s
      | App (t1, t2) -> (term_to_string t1) ^ " " ^ (term_to_string t2)
      | Lambda (typ, var, t) -> "λ" ^ var ^ ":" ^ typ ^ "." ^ (term_to_string t)
;;

let rec type_to_string typ =
    match typ with
      | Bool -> "Bool"
      | Nat -> "Nat"
;;

let print_term term =
    print_endline ((term_to_string term) ^ (type_to_string (typeof term)))
;;
