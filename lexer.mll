{
  open Parser ;;
}
rule lexer = parse                       (* nom de la fonction construite par ocamllex pour *)
                                         (* détecter des lexèmes dans un flux de caractères *)

  | [' ' '\t']          {lexer lexbuf}   (* lexème éludé ; la fonction est rappelée récursivement *)

  | "True"              {Ltrue}
  | "False"             {Lfalse}
  | "if"                {Lif}
  | "then"              {Lthen}
  | "else"              {Lelse}
  | ['0' - '9']+ as num {Lnum(int_of_string num)}
  | "succ"              {Lsucc}
  | "pred"              {Lpred}
  | "isZero"            {Lztest}

