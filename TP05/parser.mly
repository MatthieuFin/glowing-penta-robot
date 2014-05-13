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
%token Laffect /* %right? prio? */
%token Lseq
%token LunitType
%token Lin
%token Lsep
%token Lleftv
%token Lrightv
%token Lcase
%token Las
%token Lof
%token Lpipe
%token Lbarrow
%token Lletrec
%token Lref
%token Lderef


%start line                       /* axiome */
%type <Types.term> line    /* type de l'attribut de l'axiome */  

%%

line :
    | sequence    Leol                                                         {$1}
    | declare Leol                                                          {$1}

sequence:
    | superterme                                                         {$1}
    | superterme Lseq sequence 
                              {App (Lambda (UnitType, get_var_name $3, $3), $1)}

superterme :
    | terme                                                                 {$1}
    | Lletrec Lident Lequal sequence Lin sequence      {Name($2, Fix $4, $6)}
    | Llet Lident Lequal sequence Lin sequence               {Name ($2, $4, $6)}
    | terme Laffect terme                               {Affect($1,$3)}
    

terme :
    | functerm                                                              {$1} 
    | appterm functerm                                            {App ($1, $2)}

appterm :
    | valeurs                                                               {$1}
    | appterm valeurs                                              {App ($1,$2)}
   
functerm :
    | Lref sequence                                                     {Ref $2}
    | Lderef sequence                                                 {Deref $2}
    | Lsucc sequence                                                   {Succ $2}
    | Lpred sequence                                                   {Pred $2}
    | LisZero sequence                                               {IsZero $2}
    | Llambda Lident Lsemcol typage Ldot sequence            {Lambda ($4,$2,$6)}
    | Lif sequence Lthen sequence Lelse sequence             {Cond ($2, $4, $6)}
    | Lcase sequence Lof cases                                    {Case ($2,$4)}
    | valeurs                                                               {$1}
    
valeurs :
    | Lident                                                          {Var ($1)}
    | Ltrue                                                               {True}
    | Lfalse                                                             {False}
    | Lzero                                                               {Zero}
    | Lleftp sequence Lrightp                                               {$2}
    | Lunit                                                               {Unit}
    | record                                                                {$1}
    | projection                                                            {$1}
    | tag                                                                   {$1}
    
tag :
    | Lleftv Lident Lequal sequence Lrightv Las vartype       {Tag ($2, $4, $7)}

record :
    | Lleftb Lrightb                                                 {Record []}
    | Lleftb fieldlist Lrightb                                       {Record $2}
    
vartype :
    | Lleftv vartypelist Lrightv                                       {VarType $2}
    
projection :
    | valeurs Ldot Lident                                  {Projection ($1, $3)}
    
elemtype :
    | Lbool                                                               {Bool}
    | Lnat                                                                 {Nat}
    | LunitType                                                       {UnitType}
    | vartype                                                               {$1}
    | rectype                                                               {$1}
    | Lleftp typage Lrightp                                                 {$2}
    
rectype :
    | Lleftb rectypelist Lrightb                                   {RcdType($2)}
   
rectypelist :
    | Lident Lsemcol typage                                         {[($1, $3)]}
    | Lident Lsemcol typage Lsep rectypelist                      {($1, $3)::$5}

vartypelist :
    | Lident Lsemcol typage                                         {[($1, $3)]}
    | Lident Lsemcol typage Lpipe vartypelist                     {($1, $3)::$5}
    
fieldlist :
    | Lident Lequal sequence                                        {[($1, $3)]}
    | Lident Lequal sequence Lsep fieldlist                       {($1, $3)::$5}
    
typage:
    | elemtype                                                              {$1}
    | elemtype Larrow typage                                    {AppType($1,$3)}
    
cases :
    | Lleftv Lident Lequal Lident Lrightv Lbarrow sequence      
                                                                {[($2, $4, $7)]}
    | Lleftv Lident Lequal Lident Lrightv Lbarrow sequence Lpipe cases
                                                                {($2,$4,$7)::$9}
    
/* Ajout des déclarations */

declare :
    | Llet Lident Lequal sequence         {(Tools.declare $2 (Eval.examine $4 []))}


%%
 
