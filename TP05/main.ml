(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)
open Tools;;

let rec loop channel =
    begin
    try
        let t = (Parser.line Lexer.lexer channel) in
        let a = Eval.eval t in
        Output.print_term a
    with 
      | Bad_Type m -> print_endline m
      | Bad_Tag_Type (label, expected, found) -> 
        print_endline (label ^ " expected as " 
                       ^ (Output.type_to_string expected) 
                       ^ " but found as "
                       ^ (Output.type_to_string found))
      | Bad_Try (t1, t2) ->
        print_endline ((Output.type_to_string t1) ^ "//" ^ (Output.type_to_string t2))
      | Field_Not_Found str -> print_endline ("ce champ n'existe pas: " ^ str)
      | Unbound_Variable str -> print_endline ("variable inconnue: " ^ str)
      | Unbound_Alias str -> print_endline ("alias inconnue: " ^ str)
      | Unbound_Location i -> print_endline ("segfault: " ^ (string_of_int i))
      | Var_Tag_Not_Found str -> print_endline str
      | Not_Record_Throw (t, str) -> print_endline ("impossible de faire une projection sur " ^ (Output.term_to_string t))
      | Cant_Meet (t1, t2) -> print_endline ("impossible de calculer l'intersection de "
                                        ^ (Output.type_to_string t1)
                                        ^ " et " ^ (Output.type_to_string t2))
    end;
    loop channel
;;
let channel =
    match (Array.length Sys.argv) with
      | 1 -> stdin
      | 2 -> (open_in (Sys.argv.(1)))
      | _ -> failwith "Nombre de paramêtres incorrects"
in
try
    loop (Lexing.from_channel channel) 
with Lexer.Eof -> 
    close_in channel
;;
