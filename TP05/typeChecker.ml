open Types;;
open Tools;;

exception Bad_Type of string;;
exception Unbound_Alias of string;;
exception Bad_Tag_Type of string * glowyType * glowyType;;
exception Var_Tag_Not_Found of string;;
exception Not_Record_Throw of term * string;;

let rec getType var gamma = 
    let rec aux v g =
        match g with
          | [] -> typeof (getValue v) g
          | (varName, typ)::g' when varName = v -> typ
          | _::g' -> aux v g'
        in
    aux var gamma
and getRcdFieldType l gamma =
    match l with
      | [] -> []
      | (label, value):: l' -> (label, (typeof value gamma))::(getRcdFieldType l' gamma)
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
and compute_case_type case_list type_list gamma  =
    let rec aux case_list type_list gamma typ =
        match case_list with
          | [] -> typ
          | (label,alias,term)::l' 
                when (typeof term 
                        ((alias,
                             get_variant_field_type type_list label)::gamma))
                     = typ -> aux l' type_list gamma typ
          | _::l' -> raise (Bad_Type "Cas mal typé")
    in
    match case_list with
      | [] -> raise (Bad_Type "Case vide")
      | (label, alias, term)::l' 
            -> (aux l' type_list gamma 
                    (typeof term 
                        ((alias,
                             get_variant_field_type type_list label)::gamma)))
and typeof t gamma = 
    match t with
      | True -> Bool
      | False -> Bool
      | Zero -> Nat
      | Unit -> UnitType
      | Cond (c, t, f) when typeof c gamma = Bool-> begin
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
      | Name (alias, t1, t2) -> typeof t2 ((alias , typeof t1 gamma)::gamma)
      | Record l -> RcdType (getRcdFieldType l gamma)
      | Tag (label, terme, VarType lt) when (typeof terme gamma) = (get_variant_field_type lt label)  -> VarType lt
      | Tag (label, terme, VarType lt) 
            -> raise (Bad_Tag_Type (label,
                      (get_variant_field_type lt label), (typeof terme gamma)))
      | Projection (terme, label) -> begin
            let t_type = typeof terme gamma in
            match t_type with
              | RcdType l -> (get_variant_field_type l label)
              | _ -> raise (Not_Record_Throw (terme, label))
        end
      | Case (t, case_list) -> begin
            let t_type = typeof t gamma in
            match t_type with
              | VarType type_list -> compute_case_type case_list type_list gamma
              | _ -> failwith "YOLO !"
        end
      | Fix terme -> begin
            let terme_type = typeof terme gamma in
            match terme_type with
              | AppType (t1, t2) when t1 = t2-> t2
              | _ -> raise (Bad_Type "Fixe mal typé")
        end
      | _ -> raise (Bad_Type "Terme mal typé !")
;;
      
      
      
       
      
