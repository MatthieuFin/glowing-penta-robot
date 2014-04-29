/*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*/
%{
  open Types ;;
  open Tools ;;
%}

%token Leol
%token Lcomm
%token Llambda
%token Lleftp
%token Lrightp
%token Lleftb
%token Lrightb
%token Ldot
%token Lequal
%token Llet
%token Lsemcol
%token <string> Ltype
%token Ltrue
%token Lfalse
%token <string> Lident
%token Lif
%token Lthen
%token Lelse
%token Lzero
%token Lsucc
%token Lpred
%token LisZero
%token Lbool
%token Lnat
%token Larrow
%token Lunit
%token Lseq
%token LunitType
%token Lin
%token Lsep



%start line                       /* axiome */
%type <Types.term> line    /* type de l'attribut de l'axiome */  

%%

line :
    | sequence Leol            {$1}
    | declare Leol             {$1}

sequence:
    | superterme                         {$1}
    | superterme Lseq sequence {App (Lambda (UnitType, get_var_name $3, $3), $1)}

superterme :
    | terme                  {$1}
    | Llet Lident Lequal sequence Lin sequence {Name ($2, $4, $6)}

terme :
    | functerm                             {$1} 
    | appterm functerm                     {App ($1, $2)}

appterm :
    | valeurs                 {$1}
    | appterm valeurs         {App ($1,$2)}
   
functerm :
    | Lsucc sequence                                                   {Succ $2}
    | Lpred sequence                                                   {Pred $2}
    | LisZero sequence                                               {IsZero $2}
    | Llambda Lident Lsemcol typage Ldot sequence            {Lambda ($4,$2,$6)}
    | Lif sequence Lthen sequence Lelse sequence             {Cond ($2, $4, $6)}
    | valeurs                                                               {$1}
    
valeurs :
    | Lident                   {Var ($1)}
    | Ltrue                    {True}
    | Lfalse                   {False}
    | Lzero                    {Zero}
    | Lleftp sequence Lrightp      {$2}
    | Lunit                    {Unit}
    | record                   {$1}
    | projection               {$1}

record :
    | Lleftb recordfields Lrightb    {Record $2}
    
projection :
    | valeurs Ldot Lident    {Projection ($1, $3)}
    
elemtype :
    | Lbool                    {Bool}
    | Lnat                     {Nat}
    | LunitType                {UnitType}
    | Lleftp typage Lrightp    {$2}
    
    
recordfields :
    | Lident Lequal sequence                 {[($1, $3)]}
    | Lident Lequal sequence Lsep recordfields     {($1, $3)::$5}
    
typage:
    | elemtype                 {$1}
    | elemtype Larrow typage   {AppType($1,$3)}
    
/* Ajout des déclarations */

declare :
    | Llet Lident Lequal sequence    {(Tools.declare $2 $4)}


%%
 