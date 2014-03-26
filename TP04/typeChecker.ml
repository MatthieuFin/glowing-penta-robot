open Types;;
open Tools;;

exception Bad_Type;;

let decapsuleur t =
    match t with 
        | Some_type t1 -> t1
        | No_type m -> raise Bad_Type
;;

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
      | Succ (n) when (typeof n) = Some_type (Nat) -> Some_type (Nat)
      | Pred (n) when (typeof n) = Some_type (Nat) -> Some_type (Nat)
      | IsZero (n) when (typeof n) = Some_type (Nat) -> Some_type (Bool)
      | Var (s) -> 
        begin
            match (getValue s) with
              | Some (v) -> typeof v
              | None -> No_type ("Mal typé !")
        end
      | App (t1, t2) -> begin
            let type_t1 = typeof t1
            and type_t2 = typeof t2
            in match type_t1 with
                | Some_type (AppType (t11, t12)) when t11 = (decapsuleur type_t2) -> Some_type t12
                | _ -> No_type ("Mal typé")
            end
      | Lambda (typ, var, t) -> Some_type(AppType(typ, decapsuleur (typeof t)))
      | _ -> No_type ("Mal typé !")
;;
      
      
      
       
      
