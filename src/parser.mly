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
%token LET IN
%token FUN DONNE
%token IF THEN ELSE 
%token <string> NOM
%token PRINT

%token TEST


%left EGAL
%left NONEGAL
%left INF_S
%left INF_L
%left SUP_S
%left SUP_L
%left SEMICOL
%right DONNE
%left PLUS   /* associativité gauche: a+b+c, c'est (a+b)+c */
%left MOINS
%left PRINT
%left TIMES
%left DIV
%left ELSE
%right IN




%start main             /* "start" signale le point d'entrée: */
                        /* c'est ici main, qui est défini plus bas */
%type <Expr.expr> main     /* on _doit_ donner le type associé à main, qui est le point d'entrée */

%%
    /* --- début des règles de grammaire --- */
                            /* à droite, les valeurs associées */


main:                       
    simplexpr EOF               { $1 } 
;

simplexpr:
  | TEST					{TESTLINE(!line_number) }
  | PRINT simplexpr  				  { PrintInt $2 }
  | FUN NOM DONNE simplexpr                      { (Fun($2, $4)) }

  /* | simplexpr SEMICOL simplexpr		  {Seq($1, $3)} */

  | simplexpr PLUS simplexpr                    { Add($1,$3) }
  | simplexpr TIMES simplexpr                   { Mul($1,$3) }
  | simplexpr MOINS simplexpr                   { Sou($1,$3) }
  | simplexpr DIV simplexpr                     { Div($1,$3) }
  | MOINS simplexpr                             { (Sou(Const(0), $2)) }


  | LET NOM EGAL simplexpr IN simplexpr         { Let($2, $4, $6) }
  | LET NOM EGAL simplexpr IN 	        	{ Let($2, $4, Const 0)}
  | IF bexpr THEN simplexpr ELSE simplexpr      { Cond($2, $4, $6) }

  | listexpr                                    { $1 }


  /* Errors Managements */

  ;
listexpr:
  | priexpr                                     { $1 }
  | listexpr priexpr                            { App($1, $2) }
  
    
priexpr:
  | NOM	      	     		       	   	  { Identifier $1 }
  | INT                                           { Const $1 }   
  | LPAREN simplexpr RPAREN                       { $2 }
;

bexpr:
  | simplexpr EGAL simplexpr                    { Testeq($1,$3) }
  | simplexpr NONEGAL simplexpr                 { Testneq($1,$3) }
  | simplexpr INF_S simplexpr                   { Testlt($1,$3) }
  | simplexpr SUP_S simplexpr                   { Testgt($1,$3) }
  | simplexpr INF_L simplexpr                   { Testlet($1,$3) }
  | simplexpr SUP_L simplexpr                   { Testget($1,$3) }

