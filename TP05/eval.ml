(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)
open Types;;
open TypeChecker;;
open Tools;;

exception Case_Not_Found;;

let rec is_n_val t =
    match t with
      | Zero -> true
      | Succ t' -> is_n_val t'
      | Pred t' -> is_n_val t'
      | _ -> false
;;


let is_val t =
 match t with
   | Unit -> true
   | True -> true
   | False -> true
   | Lambda _ -> true
   | t' -> is_n_val t'
;;



let rec eval1 t =
    match t with
      | Unit -> t
      | True ->  t
      | False ->  t
      | Zero ->  t
      | Cond (True, t1, t2) ->  t1
      | Cond (False, t1, t2) ->  t2
      | Cond (t1, t2, t3) ->  Cond ((eval1 t1), t2, t3)
      | Succ t1 ->  Succ (eval1 t1)
      | Pred (Succ t1)  ->  t1
      | Pred Zero ->  Zero
      | Pred t1 ->  Pred (eval1 t1)
      | IsZero Zero ->  True
      | IsZero (Succ v) ->  False (*ATTENTION: on ne vérifie pas que v est une valeur *)
      | IsZero t1 ->  IsZero (eval1 t1)
      | Var x -> getValue x   
      | Lambda (ty, x, t') ->  t
      | App (Lambda(ty, x, t'), v2) when (is_val v2) -> (substitute x v2 t')
      | App (t1, t2) when (is_val t1) -> App(t1, (eval1 t2))
      | App (t1, t2) -> App ((eval1 t1), t2)
      | Name (alias, t1, t2) when (is_val t1) -> substitute alias t1 t2
      | Name (alias, t1, t2) -> Name (alias, eval1 t1, t2)
      | Record l -> Record (eval_list l)
      | Projection (Record l, label) -> find_field (eval_list l) label
      | Projection (t, label) -> Projection (eval1 t, label)
      | Tag (label, terme, typ) -> Tag (label, eval1 terme, typ)
      | Case (Tag (label, terme, typ), case_list) -> compute_cases case_list label terme
      | Case (terme, l) -> Case (eval1 terme, l)
and eval_list l = 
    match l with
        | [] -> []
        | (label, value)::l' -> (label, (examine value))::(eval_list l')
and compute_cases case_list label terme =
    match case_list with
      | [] -> raise Case_Not_Found
      | (tag_name, alias, t)::l' when tag_name = label 
        -> substitute alias terme t
      | _::l' -> compute_cases l' label terme
and examine t = 
    let t' = eval1 t in
    if t' = t then t else examine t'
;;

let eval param = 
let type_p = typeof param [] in
examine param;;
