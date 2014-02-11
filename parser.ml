type token =
  | Ltrue
  | Lfalse
  | Lif
  | Lthen
  | Lelse
  | Lzero
  | Lsucc
  | Lpred
  | LisZero
  | Leol

open Parsing;;
let _ = parse_error;;
# 7 "parser.mly"
  open Types ;;
# 18 "parser.ml"
let yytransl_const = [|
  257 (* Ltrue *);
  258 (* Lfalse *);
  259 (* Lif *);
  260 (* Lthen *);
  261 (* Lelse *);
  262 (* Lzero *);
  263 (* Lsucc *);
  264 (* Lpred *);
  265 (* LisZero *);
  266 (* Leol *);
    0|]

let yytransl_block = [|
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\006\000\002\000\002\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\003\000\000\000\004\000\000\000\000\000\
\000\000\009\000\000\000\000\000\006\000\007\000\008\000\001\000\
\000\000\000\000\000\000\005\000"

let yydgoto = "\002\000\
\010\000\011\000"

let yysindex = "\004\000\
\014\255\000\000\000\000\000\000\014\255\000\000\014\255\014\255\
\014\255\000\000\247\254\002\255\000\000\000\000\000\000\000\000\
\014\255\003\255\014\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\251\255"

let yytablesize = 23
let yytable = "\012\000\
\016\000\013\000\014\000\015\000\001\000\017\000\000\000\019\000\
\000\000\000\000\000\000\018\000\000\000\020\000\003\000\004\000\
\005\000\000\000\000\000\006\000\007\000\008\000\009\000"

let yycheck = "\005\000\
\010\001\007\000\008\000\009\000\001\000\004\001\255\255\005\001\
\255\255\255\255\255\255\017\000\255\255\019\000\001\001\002\001\
\003\001\255\255\255\255\006\001\007\001\008\001\009\001"

let yynames_const = "\
  Ltrue\000\
  Lfalse\000\
  Lif\000\
  Lthen\000\
  Lelse\000\
  Lzero\000\
  Lsucc\000\
  Lpred\000\
  LisZero\000\
  Leol\000\
  "

let yynames_block = "\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'terme) in
    Obj.repr(
# 27 "parser.mly"
                          (_1)
# 98 "parser.ml"
               : Types.terme))
; (fun __caml_parser_env ->
    Obj.repr(
# 30 "parser.mly"
                          (True)
# 104 "parser.ml"
               : 'terme))
; (fun __caml_parser_env ->
    Obj.repr(
# 31 "parser.mly"
                          (False)
# 110 "parser.ml"
               : 'terme))
; (fun __caml_parser_env ->
    Obj.repr(
# 32 "parser.mly"
                          (Zero)
# 116 "parser.ml"
               : 'terme))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'terme) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'terme) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'terme) in
    Obj.repr(
# 33 "parser.mly"
                                        (Cond (_2, _4, _6))
# 125 "parser.ml"
               : 'terme))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'terme) in
    Obj.repr(
# 34 "parser.mly"
                          (Succ _2)
# 132 "parser.ml"
               : 'terme))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'terme) in
    Obj.repr(
# 35 "parser.mly"
                          (Pred _2)
# 139 "parser.ml"
               : 'terme))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'terme) in
    Obj.repr(
# 36 "parser.mly"
                          (IsZero _2)
# 146 "parser.ml"
               : 'terme))
(* Entry line *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let line (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Types.terme)
;;
# 38 "parser.mly"
 
# 173 "parser.ml"
