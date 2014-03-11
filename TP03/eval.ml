(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)
open Types;;
open Tools;;


(* Evaluation d'un pas (small-step) *)
let rec eval1 term =
    match term with
      | Var x -> getValue x 
      | Lambda (x, t) -> term
      | App (Lambda(x, t), t2) -> (substitute x (eval1 t2) t)
      | App (t1, t2) -> App ((eval1 t1), t2)
;; 



let rec examine t = 
    match eval1 t with
        | t -> t
        | _ -> examine (eval1 t)
;;

let eval param = examine param;;
