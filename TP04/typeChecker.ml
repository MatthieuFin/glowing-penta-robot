open Types;;

let rec typeof t = 
    match t with
      | True -> Some_type (Bool)
      | False -> Some_type (Bool)
      | Zero -> Some_type (Nat)
      | Cond (c, t, f) ->
      | Succ (n) when (typeof n) = Nat -> Some_type (Nat)
      | Pred (n) when (typeof n) = Nat -> Some_type (Nat)
      | IsZero (n) when (typeof n) = Nat -> Some_type (Bool)
      | Var (s) -> 
        match (getValue s) with
          | Var (v) -> No_type ("Mal typé");
          | t' -> (typeof t')
      | App (t1, t2) -> AppType (typeof t1, typeof t2)
      | Lambda (typ, var, t) ->
      | _ -> No_type ("Mal typé !")
;;
