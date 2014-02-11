(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)

let rec loop () =
  try
    print_string ">>> " ; 
    let a = BigEval.bigExamine (Parser.line Lexer.lexer (
      (Lexing.from_string (read_line () ^"\n")))
    ) in
    Output.print_term a;
    loop ()
  with End_of_file -> ()
;;

let _ = loop () ;; 
