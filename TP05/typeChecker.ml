open Types;;
open Tools;;

let rec add_missing l1 l2 =
    match l2 with
      | [] -> l1
      | (label, value)::l2' -> begin
            let rec add_without_double l (lab, valu) =
                match l with
                  | [] -> [lab, valu]
                  | (label, value)::l' when label = lab -> l
                  | (label, value)::l' -> (label, value)::(add_without_double l' (lab, valu))
            in add_missing (add_without_double l1 (label, value)) l2'
        end
;;


let rec getType var gamma  = 
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
      | (label, value):: l' -> (label, (typeof value gamma ))::(getRcdFieldType l' gamma)
and getRcdTypeField l lab =
    match l with
      | [] -> raise (Not_found)
      | (label, typ)::l' when lab = label -> typ
      | _::l' -> getRcdTypeField l' lab
and getLocType l = try Hashtbl.find sigma l
    with Not_found -> raise (Unbound_Location l)
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
and compute_case_type case_list type_list gamma   =
    let rec aux case_list type_list gamma typ  =
        match case_list with
          | [] -> typ
          | [(label, alias, term)] when label = "_" && alias = "_" ->typeof term gamma
          | (label,alias,term)::l' 
                when (typeof term 
                        ((alias,
                             get_variant_field_type type_list label)::gamma) )
                     = typ -> aux l' type_list gamma typ 
          | _::l' -> raise (Bad_Type "Cas mal typé")
    in
    match case_list with
      | [] -> raise (Bad_Type "Case vide")
      | (label, alias, term)::l' 
            -> (aux l' type_list gamma 
                    (typeof term 
                        ((alias,
                             get_variant_field_type type_list label)::gamma) ) )
and jointype s t =
    match (s ,t) with
      | (s', t') when s' = t' -> t'
      | (AppType (sfp, sft), AppType (tfp, tft)) ->
        AppType (meettype sfp tfp, jointype sft tft)
      | (RcdType sl, RcdType tl) -> begin
            let rec joinlists l1 l2 ret =
                match l1 with
                  | [] -> ret
                  | (label, trcd1)::l1' -> begin
                        match l2 with
                          | [] -> joinlists l1' l2 ret
                          | (lab, trcd2)::l2' when lab = label -> 
                          joinlists l1' l2 ((lab, jointype trcd1 trcd2)::ret)
                          | (lab, trcd2)::l2' -> joinlists l1 l2' ret
                    end
            in RcdType (joinlists sl tl [])
        end
      | _ -> Top

and meettype s t = 
    match (s, t) with
      | (_, Top) | (Top, _) -> Top
      | (s', t') when s' = t' -> s'
      | (AppType (sfp, sft), AppType (tfp, tft)) ->
        AppType (jointype sfp tfp, meettype sft tft)
      | (RcdType sl, RcdType tl) -> begin
            let rec meet l (label, value) =
                match l with
                    | [] -> (label, value)
                    | (lab, valu)::l' when label = lab -> (lab, meettype value valu)
                    | (lab, valu)::l' -> meet l' (label, value)
            in
            let rec meetlists l1 l2 ret =
                match l1 with
                  | [] -> add_missing ret l2
                  | (label, value)::l1' -> meetlists l1' l2 ((meet l2 (label, value))::ret)
            in RcdType (meetlists sl tl [])
            end
      | _ -> raise (Cant_Meet (s, t))
        (*Indique si subtype est sous type de typ *)
and subtype s t = 
    match (s, t) with
      | (_, Top) -> true
      | (t, s) when t = s -> true
      | (AppType (s1, s2), AppType (t1, t2)) -> (subtype t1 s1) && (subtype s2 t2)
      | (RcdType sl, RcdType tl) -> try subtype_lists sl tl with Not_found -> false
      | _ -> false
and subtype_lists sl tl =
    match tl with
      | [] -> true
      | (tlabel, ttype)::tl' -> begin
(*         let ttype = try getRcdTypeField tl slabel with Not_found -> false in *)
        try (subtype (getRcdTypeField sl tlabel) ttype) &&
        (subtype_lists sl tl') with Not_found -> false
        end
      
(* si t == TOP -> vrai
  si s = t -> vrai
  si s = AppType(s1, s2) && t = AppType (t1, t2) -> subtype t1 s1 && subtype s2 t2
  
  *)
and typeof t gamma  = 
    match t with
      | True -> Bool
      | False -> Bool
      | Zero -> Nat
      | Unit -> UnitType
      | Cond (c, t, f) when typeof c gamma  = Bool-> begin
            let typeT = typeof t gamma 
            and typeF = typeof f gamma
            in let typeTF = jointype typeT typeF
	    in typeTF
        end
      | Succ (n) when (typeof n gamma ) = Nat -> Nat
      | Pred (n) when (typeof n gamma ) = Nat -> Nat
      | IsZero (n) when (typeof n gamma ) = Nat -> Bool
      | Var (s) -> getType s gamma 
      | App (t1, t2) -> begin
            let type_t1 = (typeof t1 gamma )
            and type_t2 = (typeof t2 gamma )
            in match type_t1 with
                | AppType (t11, t12) when (subtype type_t2 t11) -> t12
                | _ -> raise (Bad_Type "Application mal typée")
        end
      | Lambda (typ, var, t) -> AppType(typ, (typeof t ((var, typ)::gamma)) )
      | Name (alias, t1, t2) -> typeof t2 ((alias , typeof t1 gamma )::gamma) 
      | Record l -> RcdType (getRcdFieldType l gamma )
      | Tag (label, terme, VarType lt) when (typeof terme gamma ) = (get_variant_field_type lt label)  -> VarType lt
      | Tag (label, terme, VarType lt) 
            -> raise (Bad_Tag_Type (label,
                      (get_variant_field_type lt label), (typeof terme gamma )))
      | Projection (terme, label) -> begin
            let t_type = typeof terme gamma  in
            match t_type with
              | RcdType l -> (get_variant_field_type l label)
              | _ -> raise (Not_Record_Throw (terme, label))
        end
      | Case (t, case_list) -> begin
            let t_type = typeof t gamma  in
            match t_type with
              | VarType type_list -> compute_case_type case_list type_list gamma 
              | _ -> failwith "YOLO !"
        end
      | Fix terme -> begin
            let terme_type = typeof terme gamma  in
            match terme_type with
              | AppType (t1, t2) when t1 = t2-> t2
              | _ -> raise (Bad_Type "Fixe mal typé")
        end
      | Ref terme -> let terme_type = typeof terme gamma  in RefType terme_type
      | Deref terme -> begin
            let terme_type = typeof terme gamma  in
            match terme_type with
            | RefType gt -> gt
            | _ -> raise (Bad_Type "Deref mal typé")
        end
      | Affect (t1, t2) -> begin
            let t1_type = typeof t1 gamma  and
            t2_type = typeof t2 gamma  in
            match (t1_type,t2_type) with
            | (RefType t11, t12) when (t11 = t12) -> UnitType
            | _ -> raise (Bad_Type "Affect mal typé")
        end
      | Loc l -> let l_type = (getLocType l) in RefType l_type
      | _ -> raise (Bad_Type "Terme mal typé !")
;;
      
      
      
       
      
