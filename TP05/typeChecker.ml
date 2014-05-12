open Types;;
open Tools;;

exception Bad_Type of string;;
exception Unbound_Alias of string;;
exception Bad_Tag_Type of string * glowyType * glowyType;;
exception Var_Tag_Not_Found of string;;
exception Not_Record_Throw of term * string;;

let rec getType var gamma sigma = 
    let rec aux v g s=
        match g with
          | [] -> typeof (getValue v) g s
          | (varName, typ)::g' when varName = v -> typ
          | _::g' -> aux v g' s
        in
    aux var gamma sigma
and getRcdFieldType l gamma sigma=
    match l with
      | [] -> []
      | (label, value):: l' -> (label, (typeof value gamma sigma))::(getRcdFieldType l' gamma sigma)
and get_variant_field_type var_list alias =
    match var_list with
        | [] -> raise (Unbound_Alias alias)
        | (label, typ)::l' when label = alias -> typ
        | _::l' -> get_variant_field_type l' alias 
(* Fonction qui détermine le type d'une instruction de la forme
    case <l1 = t> as <li : T> of 
         <l1 = x1> => res1
        |<li = xi> => resi
    
    Cette fonction doit s'assurer que tout les termes pouvant être retournés
    sont du même type en considérant le type associés aux labels.
    
    Cette fonction prends en entrée une liste de triplet (label, alias, retour)
    et une liste de couples (labels, type) et un contexte.
 *)
and compute_case_type case_list type_list gamma sigma  =
    let rec aux case_list type_list gamma typ sigma =
        match case_list with
          | [] -> typ
          | (label,alias,term)::l' 
                when (typeof term 
                        ((alias,
                             get_variant_field_type type_list label)::gamma) sigma)
                     = typ -> aux l' type_list gamma typ sigma
          | _::l' -> raise (Bad_Type "Cas mal typé")
    in
    match case_list with
      | [] -> raise (Bad_Type "Case vide")
      | (label, alias, term)::l' 
            -> (aux l' type_list gamma 
                    (typeof term 
                        ((alias,
                             get_variant_field_type type_list label)::gamma) sigma) sigma)
and typeof t gamma sigma = 
    match t with
      | True -> Bool
      | False -> Bool
      | Zero -> Nat
      | Unit -> UnitType
      | Cond (c, t, f) when typeof c gamma sigma = Bool-> begin
            let typeT = typeof t gamma sigma
            and typeF = typeof f gamma sigma
            in if (typeT = typeF) then typeT else raise (Bad_Type "Condition mal typée")
        end
      | Succ (n) when (typeof n gamma sigma) = Nat -> Nat
      | Pred (n) when (typeof n gamma sigma) = Nat -> Nat
      | IsZero (n) when (typeof n gamma sigma) = Nat -> Bool
      | Var (s) -> getType s gamma sigma
      | App (t1, t2) -> begin
            let type_t1 = (typeof t1 gamma sigma)
            and type_t2 = (typeof t2 gamma sigma)
            in match type_t1 with
                | AppType (t11, t12) when t11 = type_t2 -> t12
                | _ -> raise (Bad_Type "Application mal typée")
        end
      | Lambda (typ, var, t) -> AppType(typ, (typeof t ((var, typ)::gamma)) sigma)
      | Name (alias, t1, t2) -> typeof t2 ((alias , typeof t1 gamma sigma)::gamma) sigma
      | Record l -> RcdType (getRcdFieldType l gamma sigma)
      | Tag (label, terme, VarType lt) when (typeof terme gamma sigma) = (get_variant_field_type lt label)  -> VarType lt
      | Tag (label, terme, VarType lt) 
            -> raise (Bad_Tag_Type (label,
                      (get_variant_field_type lt label), (typeof terme gamma sigma)))
      | Projection (terme, label) -> begin
            let t_type = typeof terme gamma sigma in
            match t_type with
              | RcdType l -> (get_variant_field_type l label)
              | _ -> raise (Not_Record_Throw (terme, label))
        end
      | Case (t, case_list) -> begin
            let t_type = typeof t gamma sigma in
            match t_type with
              | VarType type_list -> compute_case_type case_list type_list gamma sigma
              | _ -> failwith "YOLO !"
        end
      | Fix terme -> begin
            let terme_type = typeof terme gamma sigma in
            match terme_type with
              | AppType (t1, t2) when t1 = t2-> t2
              | _ -> raise (Bad_Type "Fixe mal typé")
        end
      | Ref terme -> let terme_type = typeof terme gamma sigma in RefType terme_type
      | Deref terme -> begin
            let terme_type = typeof terme gamma sigma in
            match terme_type with
            | RefType gt -> gt
            | _ -> raise (Bad_Type "Deref mal typé")
        end
      | Affect (t1, t2) -> begin
            let t1_type = typeof t1 gamma sigma and
            t2_type = typeof t2 gamma sigma in
            match (t1_type,t2_type) with
            | (RefType t11, t12) when (t11 = t12) -> UnitType
            | _ -> raise (Bad_Type "Affect mal typé")
        end
      | Loc l -> let l_type = sigma l in RefType l_type
      | _ -> raise (Bad_Type "Terme mal typé !")
;;
      
      
      
       
      
