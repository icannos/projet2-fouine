%{
(* --- préambule: ici du code Caml --- *)

open Expr
open Errmgr

%}
/* description des lexèmes, ceux-ci sont décrits dans lexer.mll */

%token EOF
%token EOL

%token <int> INT       /* le lexème INT a un attribut entier */
%token PLUS TIMES MOINS DIV
%token EGAL  NONEGAL INF_S INF_L SUP_S SUP_L
%token LPAREN RPAREN SEMICOL
%token LET IN REC
%token FUN DONNE
%token IF THEN ELSE 
%token <string> NOM
%token PRINT
%token REF BANG AFF

%token TEST


%right IN
%right LET

%left EGAL

%left REF
%left NONEGAL
%left INF_S
%left INF_L
%left SUP_S
%left SUP_L
%right SEMICOL
%left AFF
%right IF
%right THEN
%right ELSE
%right DONNE
%left PLUS   /* associativité gauche: a+b+c, c'est (a+b)+c */
%left MOINS
%left PRINT
%left TIMES
%left DIV




%start main 
                       
%type <Expr.expr> main    

%%

main:
  |toplevel EOF					{ $1 }
  |simplexpr EOF               			{ $1 } 
;


toplevel:   /* les let de surface */
  |binding toplevel				{ Let($1, $2) }   /* le cas de base let a = 5*/
  |binding SEMICOL SEMICOL toplevel 		{ Let($1, $4) }   /* let a = 5 ... let b = 8;; expr*/
  |binding SEMICOL SEMICOL simplexpr		{ Let($1, $4) }   /* let a = 5 ... let v = 2;; a*v */
  |binding IN toplevel	   			{ Let($1, $3) }   /* let a = 5 in 2+2 */
  |binding    					{ Let($1, Const 0)  } 

  |recursive IN toplevel			{ LetRec($1, $3) }  
  |recursive IN simplexpr			{ LetRec($1, $3) }
  |recursive SEMICOL SEMICOL toplevel		{ LetRec($1, $4) }
  |recursive SEMICOL SEMICOL simplexpr		{ LetRec($1, $4) }
  |recursive toplevel	     			{ LetRec($1, $2) }
  |recursive 					{ LetRec($1, Const 0)}

;

binding:
  |LET NOM functexpr				{($2, $3)} /*on renvoie un couple (identifier, expression)*/
;

recursive:
  |LET REC NOM functexpr			{($3, $4)}

;
  
functexpr:
  | EGAL simplexpr				{ $2 }
  | NOM functexpr                       	{ Fun($1, $2) }    
;	

simplexpr:
  | PRINT simplexpr				{ PrintInt $2 }
  | FUN NOM DONNE simplexpr                     { Fun($2, $4) }
  | simplexpr PLUS simplexpr                    { Add($1,$3) }
  | simplexpr TIMES simplexpr                   { Mul($1,$3) }
  | simplexpr MOINS simplexpr                   { Sou($1,$3) }
  | simplexpr DIV simplexpr                     { Div($1,$3) }
  | MOINS simplexpr                             { Sou(Const(0), $2) }
  | binding IN simplexpr			{ Let($1, $3) }
  | IF bexpr THEN simplexpr ELSE simplexpr      { Cond($2, $4, $6) }

  | listexpr                                    { $1 }

  | simplexpr SEMICOL simplexpr                  { Let(("_",$1),$3) }
  | NOM AFF simplexpr 				 { Aff($1, $3) }
  | REF simplexpr				 { Ref($2)     }
 


;
  
listexpr:
  | priexpr                                     { $1 }
  | listexpr priexpr                            { App($1, $2) }
  
    
priexpr:
  | NOM	      	     		       	   	{ Identifier $1 }
  | BANG NOM					 { Acc($2)     }
  | INT                                         { Const $1 }   
  | LPAREN simplexpr RPAREN                     { $2 }
;

bexpr:
  | simplexpr EGAL simplexpr                    { Testeq($1,$3) }
  | simplexpr NONEGAL simplexpr                 { Testneq($1,$3) }
  | simplexpr INF_S simplexpr                   { Testlt($1,$3) }
  | simplexpr SUP_S simplexpr                   { Testgt($1,$3) }
  | simplexpr INF_L simplexpr                   { Testlet($1,$3) }
  | simplexpr SUP_L simplexpr                   { Testget($1,$3) }

