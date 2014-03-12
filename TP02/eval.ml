(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)
open Types;;
let rec eval1 t = 
    match t with
      | True -> t
      | False -> t
      | Zero -> t
      | Cond (True, t1, t2) -> t1
      | Cond (False, t1, t2) -> t2
      | Cond (t1, t2, t3) -> Cond ((eval1 t1), t2, t3)
      | Succ t1 -> Succ (eval1 t1)
      | Pred (Succ v) when (v != True) && (v != False) -> v
      | Pred Zero -> Zero
      | Pred t1 -> Pred (eval1 t1)
      | IsZero Zero -> True
      | IsZero (Succ v) -> False (*ATTENTION: on ne vérifie pas que v est une valeur *)
      | IsZero t1 -> IsZero (eval1 t1)
;;


let rec examine t = 
    match eval1 t with
        | t -> t
        | _ -> examine (eval1 t)
;;

let eval param = examine param;;
