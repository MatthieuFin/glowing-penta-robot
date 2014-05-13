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




let rec is_val t  =
 match t with
   | Unit -> true
   | True -> true
   | False -> true
   | Lambda _ -> true
   | Tag (label, term, _) -> is_val term 
   | Record l -> is_val_record l 
   | Projection (term, label) -> is_val term 
   | Loc index -> isValLoc index 
   | t' -> is_n_val t'
and isValLoc index  =
    try let term = Hashtbl.find mu index in
    is_val term
    with Not_found -> raise (Unbound_Location index)
and is_val_record l =
    match l with
      | [] -> true
      | (_, term)::l' -> (is_val term ) && (is_val_record l' )
;;



let rec eval1 t gamma =
    match t with
      | Unit -> t
      | True ->  t
      | False ->  t
      | Zero ->  t
      | Cond (True, t1, t2) ->  t1
      | Cond (False, t1, t2) ->  t2
      | Cond (t1, t2, t3) ->  Cond ((eval1 t1 gamma), t2, t3)
      | Succ t1 ->  Succ (eval1 t1 gamma)
      | Pred (Succ t1)  ->  t1
      | Pred Zero ->  Zero
      | Pred t1 ->  Pred (eval1 t1 gamma)
      | IsZero Zero ->  True
      | IsZero (Succ v) ->  False (*ATTENTION: on ne vérifie pas que v est une valeur *)
      | IsZero t1 ->  IsZero (eval1 t1 gamma)
      | Var x -> getValue x   
      | Lambda (ty, x, t') ->  t
      | App (Lambda(ty, x, t'), v2) when (is_val v2) -> (substitute x v2 t')
      | App (t1, t2) when (is_val t1) -> App(t1, (eval1 t2 gamma))
      | App (t1, t2) -> App ((eval1 t1 gamma), t2)
      | Name (alias, t1, t2) when (is_val t1) ->  substitute alias t1 t2
      | Name (alias, t1, t2) -> Name (alias, (eval1 t1 gamma), t2)
      | Record l -> Record (eval_list l gamma)
      | Projection (Record l, label) -> find_field (eval_list l gamma) label
      | Projection (t, label) -> Projection ((eval1 t gamma), label)
      | Tag (label, terme, typ) -> Tag (label, (eval1 terme gamma), typ)
      | Case (Tag (label, terme, typ), case_list) -> compute_cases case_list label terme
      | Case (terme, l) -> Case ((eval1 terme gamma), l)
      | Fix terme when not (is_val terme) ->Fix (eval1 terme gamma)
      | Fix (Lambda(ty, label, terme)) -> 
                        substitute label (Fix (Lambda(ty, label, terme))) terme
      | Fix t -> Fix t
      | Ref t when (is_val t) -> (setRef (Hashtbl.length mu) t (typeof t gamma))
      | Ref t -> Ref (eval1 t gamma)
      | Deref Loc t when isSetRef t -> getRefValue t
      | Deref t -> Deref (eval1 t gamma)
      | Affect((Loc l), t) when (isSetRef l) && (is_val t) -> (setRef l t (typeof t gamma))
      | Affect(t1, t2) -> Affect(eval1 t1 gamma, eval1 t2 gamma)  
      | Loc t ->  Loc t   
and eval_list l gamma= 
    match l with
        | [] -> []
        | (label, value)::l' -> (label, (examine value gamma ))::(eval_list l' gamma )
and compute_cases case_list label terme =
    match case_list with
      | [] -> raise Case_Not_Found
      | [(tag, alias, seq)] when tag = "_" && alias = "_" -> seq
      | (tag_name, alias, t)::l' when tag_name = label 
        -> substitute alias terme t
      | _::l' -> compute_cases l' label terme
and examine t gamma = 
    let t' = eval1 t gamma  in
    if t' = t then t else examine t' gamma (*TODO typer et modifier gamma *)
;;

let eval param = 
    let type_p = typeof param [] in
examine param [];;
