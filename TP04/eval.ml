(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)
open Types;;
open Tools;;


(* TODO Tester eval1 *)
let rec eval1 t =
    match t with
      | True -> t
      | False -> t
      | Zero -> t
      | Cond (True, t1, t2) -> t1
      | Cond (False, t1, t2) -> t2
      | Cond (t1, t2, t3) -> Cond ((eval1 t1), t2, t3)
      | Succ t1 -> Succ (eval1 t1)
      | Pred (Succ t1)  -> t1
      | Pred Zero -> Zero
      | Pred t1 -> Pred (eval1 t1)
      | IsZero Zero -> True
      | IsZero (Succ v) -> False (*ATTENTION: on ne vérifie pas que v est une valeur *)
      | IsZero t1 -> IsZero (eval1 t1)
      | Var x -> getValue x 
      | Lambda (ty, x, t) -> t
      | App (Lambda(ty, x, t), t2) -> (substitute ty x (eval1 t2) t)
      | App (t1, t2) -> App ((eval1 t1), t2)
;; 



let rec examine t = 
    match eval1 t with
        | t -> t
        | _ -> examine (eval1 t)
;;

let eval param = examine param;;
