(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)

let rec loop channel =
    begin
    try
        let a = Eval.examine (Parser.line Lexer.lexer channel
        ) in
        Output.print_term a
    with TypeChecker.Bad_Type m ->
        print_endline m
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
