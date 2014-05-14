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
  | '#' [^'\n']* '\n'        {lexer lexbuf}
  | [' ' '\t' '\n' '\r']          {lexer lexbuf}   (* lexème éludé ; la fonction est rappelée récursivement *)
  | ";;"                {Leol}
  | ';'                 {Lseq}
  | '('                 {Lleftp}
  | ')'                 {Lrightp}
  | '{'                 {Lleftb}
  | '}'                 {Lrightb}
  | ','                 {Lsep}
  | '<'                 {Lleftv}
  | '>'                 {Lrightv}
  | "->"                {Larrow}
  | "=>"                {Lbarrow}
  | '.'                 {Ldot}
  | '|'                 {Lpipe}
  | '_'                 {LdefaultC}
  | '*'                 {Lstar}
  | "letrec"            {Lletrec}
  | "let"               {Llet}
  | '='                 {Lequal}
  | "lambda"            {Llambda}
  | ':'                 {Lsemcol}
  | eof                 {raise Eof}
  | "true"              {Ltrue}
  | "false"             {Lfalse}
  | "if"                {Lif}
  | "then"              {Lthen}
  | "else"              {Lelse}
  | '0'                 {Lzero}
  | "succ"              {Lsucc}
  | "pred"              {Lpred}
  | "isZero"            {LisZero}
  | "Bool"              {Lbool}
  | "Nat"               {Lnat}
  | "Unit"              {LunitType}
  | "as"                {Las}
  | "of"                {Lof}
  | "()"                {Lunit}
  | "in"                {Lin}
  | "case"              {Lcase}
  | ":="                {Laffect}
  | "ref"               {Lref}
  | "Ref"               {Lref}
  | "deref"             {Lderef}
  | "!"                 {Lderef}
  | ['a'-'z'] ['a' - 'z' 'A'-'Z' '0'-'9']*  {Lident (Lexing.lexeme lexbuf)}
  | _ as c              {(Printf.printf "Erreur : %c\n" c);failwith "lexème inattendu"}

