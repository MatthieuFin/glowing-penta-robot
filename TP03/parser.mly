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
%token Ldot
%token Lequal
%token Llet
%token <string> Lident

%start line                       /* axiome */
%type <Types.term> line    /* type de l'attribut de l'axiome */  

%%

line :
    | term Leol            {$1}
    | declare Leol         {$1}

term :
    | functerm                 {$1} 
    | appterm functerm         {App ($1, $2)}

appterm :
    | elemterm                 {$1}
    | appterm elemterm         {App ($1,$2)}
   
functerm :
    | Llambda Lident Ldot term  {Lambda ($2,$4)}
    | elemterm                 {$1} 

elemterm :
    | Lident                   {Var ($1)}
    | Lleftp term Lrightp             {$2}
    
/* Ajout des déclarations */

declare :
    | Llet Lident Lequal term    {(Tools.declare $2 $4)}

%%
 
