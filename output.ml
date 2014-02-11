open Types;;
open Eval;;

let print_term param = 
    let rec aux term = 
        match term with
            | True -> "True"
            | False -> "False"
            | Zero -> "Zero"
            | Cond (t1, t2, t3) -> "If (" ^ (aux t1) ^ ")\n\tthen " ^ (aux t2)
                                ^ "\n\t" ^ (aux t3)
            | Succ t -> "Succ(" ^ (aux t) ^ ")"
            | Pred t -> "Pred(" ^ (aux t) ^ ")"
            | IsZero t -> "isZero(" ^ (aux t) ^ ")"
    in print_endline (aux param)
;;
