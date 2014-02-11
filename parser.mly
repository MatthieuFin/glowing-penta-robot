%{
  open Types ;;
%}

%token Ltrue
%token Lfalse
%token Lif
%token Lthen
%token Lelse
%token Lzero
%token Lsucc
%token Lpred
%token LisZero

%start line                       /* axiome */
%type <Types.terme> line    /* type de l'attribut de l'axiome */  

%%

line :
  | prop Leol            {$1}

prop :
  | prop Lequiv prop     {Equiv ($1, $3)}
  | prop Linvolve prop   {Involve ($1, $3)}
  | prop Lor prop        {Or ($1, $3)}
  | prop Land prop       {And ($1, $3)}
  | Lnot prop            {Not $2}
  | Llpar prop Lrpar     {$2}
  | Ltrue                {True}
  | Lfalse               {False}
  | Lident               {Var $1}

%%
 
