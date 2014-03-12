(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)

{
  open Parser ;;
  exception Eof;;
}
rule lexer = parse                       (* nom de la fonction construite par ocamllex pour *)
                                         (* détecter des lexèmes dans un flux de caractères *)

  | [' ' '\t']          {lexer lexbuf}   (* lexème éludé ; la fonction est rappelée récursivement *)
  | '\r'?'\n'           {Leol}
  | "(*" [^'\n']* "*)"  {lexer lexbuf}               
  | '#' [^'\n']* '\n' ? {lexer lexbuf}
  | '('                 {Lleftp}
  | ')'                 {Lrightp}
  | '.'                 {Ldot}
  | "let"               {Llet}
  | '='                 {Lequal}
  | "lambda"            {Llambda}
  | ':'                 {Lsemcol}
  | ['a'-'z' 'A'-'Z']+  {Lident (Lexing.lexeme lexbuf)}
  | eof                 {raise Eof}
  | "True"              {Ltrue}
  | "False"             {Lfalse}
  | "if"                {Lif}
  | "then"              {Lthen}
  | "else"              {Lelse}
  | "Zero"              {Lzero}
  | "succ"              {Lsucc}
  | "pred"              {Lpred}
  | "isZero"            {LisZero}
  | "Bool"              {Lbool}
  | "Nat"               {Lnat}
  | _ as c              {(Printf.printf "Erreur : %c" c);Leol}

