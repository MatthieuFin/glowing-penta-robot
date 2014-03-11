(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)
open Types;;
open Tools;;


(* TODO Evaluation d'un pas (small-step) *)
let rec eval1 term = term
;; 



let rec examine t = 
    match eval1 t with
        | t -> t
        | _ -> examine (eval1 t)
;;

let eval param = examine param;;
