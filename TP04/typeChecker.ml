open Types;;

let rec typeof t = 
    match t with
      | True -> Some_type (Bool)
      | False -> Some_type (Bool)
      | Zero -> Some_type (Nat)
      | Cond (c, t, f) when typeof(c) = Some_type (Bool)-> 
        begin
            let typeT = typeof(t)
            and typeF = typeof(f)
            in if (typeT = typeF) then typeT else No_type ("Mal typé")
        end
      | Succ (n) when (typeof n) = Nat -> Some_type (Nat)
      | Pred (n) when (typeof n) = Nat -> Some_type (Nat)
      | IsZero (n) when (typeof n) = Nat -> Some_type (Bool)
      | Var (s) -> 
        begin
            match (getValue s) with
              | Var (v) -> No_type ("Mal typé");
              | t' -> (typeof t')
        end
      | App (t1, t2) -> AppType (typeof t1, typeof t2)
      | Lambda (typ, var, t) -> 
      | _ -> No_type ("Mal typé !")
;;
