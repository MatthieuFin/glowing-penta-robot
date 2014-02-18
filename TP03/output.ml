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

(*TODO affichage du terme*)
let print_term param =
    match param with 
        | Var x  -> print_string x
        | _  -> print_string "autre";;
