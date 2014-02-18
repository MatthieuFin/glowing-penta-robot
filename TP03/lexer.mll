(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)
{
  open Parser ;;
}
rule lexer = parse                       (* nom de la fonction construite par ocamllex pour *)
                                         (* détecter des lexèmes dans un flux de caractères *)

  | [' ' '\t']          {lexer lexbuf}   (* lexème éludé ; la fonction est rappelée récursivement *)
  | '\n'                {Leol}
  | "(*" [^'\n']* "*)"  {lexer lexbuf}               (*TODO faire marcher les commentaires*)
  | '('                 {Lleftp}
  | ')'                 {Lrightp}
  | '.'                 {Ldot}
  | "let"               {Llet}
  | '='                 {Lequal}
  | "lambda"            {Llambda}
  | ['a'-'z' 'A'-'Z']+  {Lident (Lexing.lexeme lexbuf)}
  | _ as c              {(Printf.printf "Erreur : %c" c);Leol}
