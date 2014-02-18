(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)
open Types;;
open Tools;;


(*TODO Ca marche ce truc? qui l'a fait?*)
let rec eval1 term =
    match term with
      | Var x -> getValue x
      | Lambda (x, t) -> term
      | App (Lambda(x, t), t2) -> (substitute x (eval1 t2) t)
      | App (t1, t2) -> App ((eval1 t1), t2)
;; 


(*TODO La boucle sur les small step et l'evaluation call by value*)
let examine t = eval1 t;;
