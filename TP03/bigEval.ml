(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)
open Types;;

(*TODO< Évaluation bigStep callbyvalue d'un terme *)
let rec bigEval1 term = Var ""
;;


let rec bigExamine param = bigEval1 param;;

let bigEval param = bigExamine param;;
