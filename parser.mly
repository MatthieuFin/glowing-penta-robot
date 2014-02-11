/*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*/
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
%token Leol

%start line                       /* axiome */
%type <Types.terme> line    /* type de l'attribut de l'axiome */  

%%

line :
  | terme Leol            {$1}

terme :
    | Ltrue               {True}
    | Lfalse              {False}
    | Lzero               {Zero}
    | Lif terme Lthen terme Lelse terme {Cond ($2, $4, $6)}
    | Lsucc terme         {Succ $2}
    | Lpred terme         {Pred $2}
    | LisZero terme       {IsZero $2}
%%
 
