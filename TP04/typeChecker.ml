open Types;;
open Tools;;

exception Bad_Type;;

let decapsuleur t =
    match t with 
        | Some_type t1 -> t1
        | No_type m -> raise Bad_Type
;;

let rec getType var gamma = 
    let rec aux v g =
        match g with
          | [] -> begin
             match getValue v with
                | Some t -> typeof t g
                | None -> No_type ("Variable non typée")
            end
          | (varName, typ)::g' when varName = v -> typ
          | _::g' -> aux v g'
        in
    aux var gamma
and typeof t gamma = 
    match t with
      | True -> Some_type (Bool)
      | False -> Some_type (Bool)
      | Zero -> Some_type (Nat)
      | Cond (c, t, f) when typeof c gamma = Some_type (Bool)-> 
        begin
            let typeT = typeof t gamma
            and typeF = typeof f gamma
            in if (decapsuleur typeT = decapsuleur typeF) then typeT else No_type ("Condition mal typée")
        end
      | Succ (n) when (typeof n gamma) = Some_type (Nat) -> Some_type (Nat)
      | Pred (n) when (typeof n gamma) = Some_type (Nat) -> Some_type (Nat)
      | IsZero (n) when (typeof n gamma) = Some_type (Nat) -> Some_type (Bool)
      | Var (s) -> getType s gamma
      | App (t1, t2) -> begin
            let type_t1 = (typeof t1 gamma)
            and type_t2 = (typeof t2 gamma)
            in match type_t1 with
                | Some_type (AppType (t11, t12)) when t11 = (decapsuleur type_t2) -> Some_type t12
                | _ -> No_type ("Application mal typée")
            end
      | Lambda (typ, var, t) -> Some_type(AppType(typ, decapsuleur (typeof t ((var, Some_type typ)::gamma))))
      | _ -> No_type ("Terme mal typé !")
;;
      
      
      
       
      
