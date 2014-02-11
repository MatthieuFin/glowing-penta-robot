(*
  Auteurs:
    | Damien Picard 
    | Benjamin Zigh
*)
exception NotTauto of string;;

(*
  Type utilisé pour représenter une formule:
  Une formule est soit un symbole sous forme de chaîne
  de caractères soit un opérateur et deux formules.
*)
type formule =
     Bottom
    | Symbole of string
    | Non of formule 
    | Et of (formule * formule)
    | Ou of (formule * formule)
    | Implique of (formule * formule) 
    | Equivalent of (formule * formule)
;;

(* Donne la valeur d'une variable liée a une valeur dans la liste *)
let rec eval_variable v l = match l with
     (x, true)::ll when x = v -> Implique (Bottom , Bottom)
    | (x, false)::ll when x = v -> Bottom
    | x::ll -> (eval_variable v ll)
    | [] -> failwith ("Symbole sans valeur :" ^ v)
;;

(* Retourne une formule comme une chaîne de caractères *)
let rec formule_to_string f = match f with
     Bottom ->  "Bottom"
    | Symbole s -> s
    | Non p -> "non " ^ formule_to_string p
    | Et (p, q) -> "(" ^ formule_to_string p ^ " et " ^ formule_to_string q ^ ")"
    | Ou (p, q) -> "(" ^ formule_to_string p ^ " ou " ^ formule_to_string q ^ ")"
    | Implique (p, q) -> "(" ^ formule_to_string p ^ " => " ^ formule_to_string q ^ ")"
    | Equivalent (p, q) -> "(" ^ formule_to_string p ^ " <=> " ^ formule_to_string q ^ ")"
;;

(*
  Évalue la valeur d'une formule selon une liste de
  variables liées a des valeurs booléennes.
*)
let rec eval_formule f l = match f with
     Bottom -> false
    | Implique(Bottom, Bottom) -> true
    | Symbole s -> (eval_formule (eval_variable s l) l)
    | Non p -> (not (eval_formule p l))
    | Et (p, q) -> ((eval_formule p l) && (eval_formule q l))
    | Ou (p, q) -> ((eval_formule p l) || (eval_formule q l))
    | Implique (p, q) -> ((not (eval_formule p l)) || (eval_formule q l))
    | Equivalent (p, q) -> ((eval_formule p l) == (eval_formule q l))
;;

(* Indique si un élément appartient à la liste *)
let rec is_inside e l = match l with
   [] -> false
  |x::ll when x = e -> true
  | _::ll -> is_inside e ll
;;

(* Supprime tout les doublons d'une liste *)
let clean l = 
    let rec aux ll acc = match ll with
     [] -> List.rev acc
    | x::lll when is_inside x acc -> aux lll acc 
    | x::lll -> aux lll (x::acc) in
  aux l []
;;    

(* Ajoute un élément à une liste si il n'y est pas déja *)
let rec clean_add l1 l2 = 
    let rec aux ll1 acc = match ll1 with
         [] -> acc
        | x::ll when is_inside x acc -> (aux ll acc)
        | x::ll -> (aux ll (x::acc)) in
        List.rev (aux l1 l2)
;;

(* Donne la liste des symboles dans une formule *)
let symbols_list f =
    let rec aux ff l = match ff with
         Bottom -> l
        | Symbole s -> (s::l)
        | Non g -> (aux g l)
        | Et (g, h) ->  (clean_add (aux g l) (aux h []))
        | Ou (g, h) ->  (clean_add (aux g l)(aux h []))
        | Implique (g, h) ->  (clean_add (aux g l) (aux h []))
        | Equivalent (g, h) -> (clean_add (aux g l) (aux h [])) in
        (clean (List.rev (aux f [])))
;;

(* Affiche la liaison sous forme de chaîne de caractères *)
let liaison_to_string l = match l with
     [] -> "\n"
    | _ -> let rec aux ll acc = match ll with
                [] -> acc
               | (s,b)::lll when (b == true) -> aux lll (s ^ " est vraie\n" ^ acc)
               | (s,b)::lll -> aux lll (s ^ " est fausse\n" ^ acc) in
           (" quand \n" ^ (aux l ""))
;;

(* Donne un quantificateur sous forme de chaîne de caractères *)
let quant_to_string l  = match l with
     [] -> "\n"
    | s::[] -> ("pour toute proposition " ^ s ^ "\n")
    | _ -> let rec aux ll acc = match ll with
                [] -> acc
               | s::lll -> aux lll (s ^ "," ^ acc) in
           (" pour toutes propositions " ^ (aux l "") ^ "\n")
;;

(*
  Indique si la formule est une tautologie
  Lève une exception si ce n'est pas le cas en indiquant
  quelle combinaison refute cette proposition.
*)

let is_tautologie f = 
    let rec aux f l sl = match sl with
        [] when (eval_formule f l) = false -> raise (NotTauto ((formule_to_string f) 
        ^ " n'est pas un theoreme,\n car la proposition est fausse" 
        ^ (liaison_to_string l)))
        | [] -> eval_formule f l
        | x::ll -> (aux f ((x, true)::l) ll) && (aux f ((x, false)::l) ll) in
        (aux f [] (symbols_list f))
;;

(*
  Affiche si oui ou non la formule est une tautologie est si non
  indique une liaison qui permet de refuter la formule
*)
let print_tautologie f =
    try 
    if ((is_tautologie f) = true) then
        (print_endline ("Theoreme :" ^ (quant_to_string (symbols_list f)) ^ (formule_to_string f) ^ "\n"))
    with NotTauto mess -> print_endline mess;;
        

(*****************************************************************************)
(*                                   TEST                                    *)
(*****************************************************************************)
let b = Bottom;;

let f = Implique(b,b);;

let g = Implique(b,f);;

let h = Equivalent(Non(Et(Symbole "p",Symbole "q")), Ou(Non(Symbole "p"),Non(Symbole "q")));;

let i = Implique(f,b);;

let j = Equivalent(Implique(Implique(Symbole "p", Symbole "q"), Symbole "r"), Implique(Symbole "p", Implique(Symbole "q", Symbole "r")));;

let k = Ou(Symbole "p", Non(Symbole "p"));;

print_tautologie g;;

print_tautologie k;;

print_tautologie h;;

print_tautologie i;;

print_tautologie j;;












