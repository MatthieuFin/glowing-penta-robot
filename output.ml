open Types;;
open Eval;;

let getIndent i =
  let rec aux i res =
    match i with
      | 0 -> res
      | _ -> aux (i - 1) res ^ "    "
  in
    aux i ""
;;

let print_term param = 
  let rec aux term i  =
    match term with
      | Zero -> (getIndent i) ^ "Zero\n"
      | True -> (getIndent i) ^ "True\n"
      | False -> (getIndent i) ^ "False\n"
      | Cond (t1, t2, t3) ->
          (getIndent i) ^ "if\n" ^ (aux t1 (i + 1)) 
          ^ (getIndent i) ^ "then\n" ^ (aux t2 (i + 1))
          ^ (getIndent i) ^ "else\n" ^ (aux t3 (i + 1))
      | Succ t -> (getIndent i) ^ "succ\n" ^ (aux t (i + 1))
      | Pred t -> (getIndent i) ^ "pred\n" ^ (aux t (i + 1))
      | IsZero t -> (getIndent i) ^ "isZero\n" ^ (aux t (i + 1))
  in
    print_string (aux param 0)
;;
