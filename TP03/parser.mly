/*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*/
%{
  open Types ;;
%}

%token Leol
%token Llambda
%token Lleftp
%token Lrightp
%token <string> Lident

%start line                       /* axiome */
%type <Types.term> line    /* type de l'attribut de l'axiome */  

%%

line :
    | functerm Leol            {$1}  
    
    
functerm :
    | term Llambda Lident functerm  {Lambda ($3,$4)}
    | term           {$1}
    


    
term :
    | elemterm                 {$1}
    | term elemterm            {App ($1,$2)}
    |                           {}

elemterm :
    | Lident               {Var $1}



    
    

%%
 
