(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)
open Types;;
open Tools;;

let rec eval1 term =
    match term with
      | Var x -> getValue x
      | Lambda (x, t) -> term
      | App (Lambda(x, t), t2) -> (substitute x (eval1 t2) t)
      | App (t1, t2) -> App ((eval1 t1), t2)
;; 
