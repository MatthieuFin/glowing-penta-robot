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

let rec list_type_to_string l =
    match l with
      | [] -> ""
      | (tag, value)::[] -> tag ^ " : " ^ (type_to_string value) 
      | (tag, value)::l' -> tag ^ " : " ^ (type_to_string value)  
                          ^ ", " ^ (list_type_to_string l')
and type_to_string typ =
    match typ with
      | UnitType -> "Unit"
      | Bool -> "Bool"
      | Nat -> "Nat"
      | AppType (t1,t2) -> (type_to_string t1)^" -> "^(type_to_string t2)
      | RcdType l -> "{" ^ (list_type_to_string l) ^ "}"
      | VarType l -> "<" ^ (list_type_to_string l) ^ ">"
;;

let rec list_to_string l = 
    match l with
      | [] -> ""
      | (tag, value)::[] -> tag ^ " = " ^ (term_to_string value) 
      | (tag, value)::l' -> tag ^ " = " ^ (term_to_string value)  
                          ^ ", " ^ (list_to_string l')
and case_list_to_string l =
    match l with
      | [] -> ""
      | [(label, alias, terme)] -> 
            ("<" ^ label ^ " = " ^ alias ^"> => " ^ (term_to_string terme))
      | (label, alias, terme):: l' 
            -> ("<" ^ label ^ " = " ^ alias ^"> => " ^ (term_to_string terme)
               ^ " | " ^ (case_list_to_string l'))
and term_to_string term =
    match term with
      | Unit -> "()"
      | True -> "True"
      | False -> "False"
      | Zero -> "0"
      | Cond (c, t, f) -> "If (" ^ (term_to_string c) ^ ") Then "
                          ^ (term_to_string t) 
                          ^ "Else " 
                          ^ (term_to_string f)
      | Succ (n) -> "Succ " ^ (term_to_string n)
      | Pred (n) -> "Pred " ^ (term_to_string n)
      | IsZero (n) -> "IsZero " ^ (term_to_string n)
      | Var (s) -> s
      | App (t1, t2) -> (term_to_string t1) ^ " " ^ (term_to_string t2)
      | Lambda (typ, var, t) -> "λ" ^ var ^ " : " ^ (type_to_string typ) ^ ". " ^ (term_to_string t)
      | Name (alias, t1, t2) -> "let " ^ alias 
                              ^ " = " ^ (term_to_string t1) 
                              ^ " in " ^ (term_to_string t2)
      | Record l -> "{" ^ (list_to_string l) ^ "}"
      | Tag (label, terme, typ) -> "<" ^ label ^" = " 
                                    ^ term_to_string terme ^ "> as " ^ (type_to_string typ)
      | Projection (t, l) -> (term_to_string t) ^ "." ^ l
      | Case (t, l) -> "case " ^ (term_to_string t) ^ " of " ^ (case_list_to_string l)
      | Fix t -> term_to_string t
;;


let print_term term =
    print_endline ((term_to_string term) ^ " : " ^ (type_to_string (typeof term [] [])))
;;
