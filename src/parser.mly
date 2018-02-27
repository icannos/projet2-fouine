%{
(* --- préambule: ici du code Caml --- *)

open Expr   

%}
/* description des lexèmes, ceux-ci sont décrits dans lexer.mll */

%token EOF
%token EOL

%token <int> INT       /* le lexème INT a un attribut entier */
%token PLUS TIMES MOINS DIV
%token EGAL  NONEGAL INF_S INF_L SUP_S SUP_L
%token LPAREN RPAREN SEMICOL
%token LET IN
/* %token FUN DONNE */
%token IF THEN ELSE 
%token <string> NOM
%token PRINT


%left EGAL
%left NONEGAL
%left INF_S
%left INF_L
%left SUP_S
%left SUP_L
%left SEMICOL
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
    expression EOF               { $1 } 
;
expression:/* règles de grammaire pour les expressions */
  | NOM	      	     		       	   	  { Identifier $1 }
  | INT                                           { Const $1 }
  | LPAREN expression RPAREN                      { $2 }
  | PRINT expression  				  { PrintInt $2 }

  /* | expression SEMICOL expression		  {Seq($1, $3)} */

  | expression PLUS expression                    { Add($1,$3) }
  | expression TIMES expression                   { Mul($1,$3) }
  | expression MOINS expression                   { Sou($1,$3) }
  | MOINS expression                              { Sou( Const(0), $2) }
  | expression DIV expression                     { Div($1,$3) }

  | LET NOM EGAL expression IN expression         { Let($2, $4, $6) }
  | LET NOM EGAL expression IN 			  { Let($2, $4, Const 0)}
  | IF expression THEN expression ELSE expression { Cond($2, $4, $6) }

  | expression EGAL expression                    { Testeq($1,$3) }
  | expression NONEGAL expression                 { Testneq($1,$3) }
  | expression INF_S expression                   { Testlt($1,$3) }
  | expression SUP_S expression                   { Testgt($1,$3) }
  | expression INF_L expression                   { Testlet($1,$3) }
  | expression SUP_L expression                   { Testget($1,$3) }

  /* Errors Managements */

;

