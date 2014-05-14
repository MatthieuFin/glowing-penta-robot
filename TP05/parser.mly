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
%token Lreft
%token Lderef
%token LdefaultC
%token Ltop

%start line                       /* axiome */
%type <Types.term> line    /* type de l'attribut de l'axiome */  

%right Lbarrow
%left Lpipe
%%

line :
    | sequence Leol                                                        {$1}
    | declare  Leol                                                        {$1}

sequence:
    | superterme                                                           {$1}
    | superterme Lseq sequence 
                              {App (Lambda (UnitType, get_var_name $3, $3), $1)}

superterme :
    | terme                                                                 {$1}
    | Lletrec Lident Lsemcol typage Lequal superterme Lin superterme      
                                        {Name($2, Fix (Lambda($4, $2, $6)), $8)}
    | Llet Lident Lequal superterme Lin superterme           {Name ($2, $4, $6)}
    | valeurs Laffect superterme                               {Affect($1,$3)}
    

terme :
    | functerm                                                              {$1} 
    | appterm functerm                                            {App ($1, $2)}

appterm :
    | valeurs                                                               {$1}
    | appterm valeurs                                              {App ($1,$2)}
   
functerm :
    | Lref terme                                                     {Ref $2}
    | Lderef terme                                                 {Deref $2}
    | Lsucc terme                                                   {Succ $2}
    | Lpred terme                                                   {Pred $2}
    | LisZero terme                                               {IsZero $2}
    | Llambda Lident Lsemcol typage Ldot superterme            {Lambda ($4,$2,$6)}
    | Lif terme Lthen terme Lelse terme             {Cond ($2, $4, $6)}
    | Lcase terme Lof  cases                              {Case ($2,$4)}
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
    | Lleftv Lident Lequal superterme Lrightv Las vartype       {Tag ($2, $4, $7)}

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
    | Ltop                                                                 {Top}
    | LunitType                                                       {UnitType}
    | vartype                                                               {$1}
    | rectype                                                               {$1}
    | Lleftp typage Lrightp                                                 {$2}

    
rectype :
    | Lleftb Lrightb                                                 {RcdType []}
    | Lleftb rectypelist Lrightb                                   {RcdType($2)}
   
rectypelist :
    | Lident Lsemcol typage                                         {[($1, $3)]}
    | Lident Lsemcol typage Lsep rectypelist                      {($1, $3)::$5}

vartypelist :
    | Lident Lsemcol typage                                         {[($1, $3)]}
    | Lident Lsemcol typage Lpipe vartypelist                     {($1, $3)::$5}
    
fieldlist :
    | Lident Lequal superterme                                        {[($1, $3)]}
    | Lident Lequal superterme Lsep fieldlist                       {($1, $3)::$5}
    
typage:
    | elemtype                                                              {$1}
    | Lreft typage                                                    {RefType $2}
    | elemtype Larrow typage                                    {AppType($1,$3)}
    
cases :
    | Lpipe LdefaultC Lbarrow superterme                            {[("_", "_", $4)]}
    | Lpipe Lleftv Lident Lequal Lident Lrightv Lbarrow superterme      
                                                                {[($3, $5, $8)]}
    | Lpipe Lleftv Lident Lequal Lident Lrightv Lbarrow superterme cases
                                                                {($3,$5,$8)::$9}
    
/* Ajout des déclarations */

declare :
    | Llet Lident Lequal superterme   {(Tools.declare $2 (Eval.examine $4  []))}


%%
 
