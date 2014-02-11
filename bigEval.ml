(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)
open Types;;

let rec bigEval1 term = 
    match term with
        | True -> term
        | False -> term
        | Zero -> term
        | Cond(t1, t2, t3) when ((bigEval1 t1) == True) -> bigEval1 t2
        | Cond(t1, t2, t3) when ((bigEval1 t1 ) == False) -> bigEval1 t3
        | Succ t -> Succ (bigEval1 t)
        | Pred t  -> (match (bigEval1 t) with 
                        | Zero -> Zero
                        | Succ tt -> tt
                        | _ -> t
                        )
        | IsZero t -> (match (bigEval1 t) with
                        | Zero -> True
                        | Succ tt -> False
                        | _ -> t
                        )
        | _ -> term
;;


let rec bigExamine param = bigEval1 param;;

let bigEval param = bigExamine param;;
