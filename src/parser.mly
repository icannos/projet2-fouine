%{
(* --- préambule: ici du code Caml --- *)

open Expr   (* rappel: dans expr.ml: 
             type expr = Const of int | Add of expr*expr | Mull of expr*expr *)

%}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <int> INT       /* le lexème INT a un attribut entier */
%token PLUS TIMES MOINS DIV
%token EGAL
%token LPAREN RPAREN
%token LET IN
%token <string> NOM
%token EOL             /* retour à la ligne */


%left PLUS   /* associativité gauche: a+b+c, c'est (a+b)+c */
%left MOINS
%left TIMES
%left DIV
/*aucune idee de l'associativite des let*/


%start main             /* "start" signale le point d'entrée: */
                        /* c'est ici main, qui est défini plus bas */
%type <Expr.expr> main     /* on _doit_ donner le type associé à main, qui est le point d'entrée */

%%
    /* --- début des règles de grammaire --- */
                            /* à droite, les valeurs associées */


main:                       /* <- le point d'entrée (cf. + haut, "start") */
    expression EOL                { $1 }  /* on veut reconnaître une "expression" */
;
expression:			    /* règles de grammaire pour les expressions */
  | INT                                         { Const $1 }
  | LPAREN expression RPAREN                    { $2 } /* on récupère le deuxième élément */
  | expression PLUS expression                  { Add($1,$3) }
  | expression TIMES expression                 { Mul($1,$3) }
  | expression MOINS expression                 { Sou($1,$3) }
  | MOINS expression                            { Sou( Const(0), $2) }
  | expression DIV expression                   { Div($1,$3) }/*en fait c'est non demandé, je le vire peut être */
/*  | LET NOM EGAL expression IN expression       {à remplir, il faut remplacer les occurrences, comment on fait ça... }*/
;

