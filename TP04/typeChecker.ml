open Types;;
open Tools;;

exception Bad_Type of string;;

let rec getType var gamma = 
    let rec aux v g =
        match g with
          | [] -> begin
             match getValue v with
                | Some t -> typeof t g
                | None -> raise (Bad_Type "Variable non typée")
            end
          | (varName, typ)::g' when varName = v -> typ
          | _::g' -> aux v g'
        in
    aux var gamma
and typeof t gamma = 
    match t with
      | True -> Bool
      | False -> Bool
      | Zero -> Nat
      | Cond (c, t, f) when typeof c gamma = Bool-> 
        begin
            let typeT = typeof t gamma
            and typeF = typeof f gamma
            in if (typeT = typeF) then typeT else raise (Bad_Type "Condition mal typée")
        end
      | Succ (n) when (typeof n gamma) = Nat -> Nat
      | Pred (n) when (typeof n gamma) = Nat -> Nat
      | IsZero (n) when (typeof n gamma) = Nat -> Bool
      | Var (s) -> getType s gamma
      | App (t1, t2) -> begin
            let type_t1 = (typeof t1 gamma)
            and type_t2 = (typeof t2 gamma)
            in match type_t1 with
                | AppType (t11, t12) when t11 = type_t2 -> t12
                | _ -> raise (Bad_Type "Application mal typée")
            end
      | Lambda (typ, var, t) -> AppType(typ, (typeof t ((var, typ)::gamma)))
      | _ -> raise (Bad_Type "Terme mal typé !")
;;
      
      
      
       
      
