(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)

let rec loop channel =
  try
    let a = Eval.examine (Parser.line Lexer.lexer ((Lexing.from_channel channel))
    ) in
    Output.print_term a;
    loop channel
  with Lexer.Eof -> 
    close_in channel;
;;
let channel =
    match (Array.length Sys.argv) with
      | 1 -> stdin
      | 2 -> (open_in (Sys.argv.(1)))
      | _ -> failwith "Nombre de paramêtres incorrects"
in
loop channel ;; 
