(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)
open Types;;
open TypeChecker;;
open Tools;;

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
      | Var x -> 
            begin
                match getValue x  with 
                    | None -> failwith ("Variable inconnue " ^ x)
                    | Some t' -> t'
            end
      | Lambda (ty, x, t') ->  t
      | App (Lambda(ty, x, t'), v2) when (is_val v2) -> (substitute x v2 t')
      | App (t1, t2) when (is_val t1) -> App(t1, (eval1 t2))
      | App (t1, t2) -> App ((eval1 t1), t2)
      | Name (alias, t1, t2) when (is_val t1) -> substitute alias t1 t2
      | Name (alias, t1, t2) -> Name (alias, eval1 t1, t2)
      | Record l -> Record (eval_record l)
      | Projection (Record l, label) -> find_field l label
      | Projection (_, _) -> t
and eval_record l = 
    match l with
        | [] -> []
        | (label, value)::l' -> (label, (examine value))::(eval_record l')
and examine t = 
    let t' = eval1 t in
    if t' = t then t else examine t'
;;

let eval param = 
let type_p = typeof param [] in
examine param;;
