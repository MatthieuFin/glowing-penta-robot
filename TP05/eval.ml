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
   | True -> true
   | False -> true
   | Lambda _ -> true
   | t' -> is_n_val t'
;;

(* TODO Tester eval1 *)
let rec examine t = 
    let t' = eval1 t in
    if t' = t then t else examine t'
and eval1 t =
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
      | App (Lambda(ty, x, t'), v2) when (is_val v2) -> (substitute ty x v2 t')
      | App (t1, t2) when (is_val t1) -> App(t1, (eval1 t2))
      | App (t1, t2) -> App ((eval1 t1), t2)
;; 

let eval param = 
let type_p = typeof param [] in
examine param;;
