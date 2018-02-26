%{
(* --- pr�ambule: ici du code Caml --- *)

open Expr   

%}
/* description des lex�mes, ceux-ci sont d�crits dans lexer.mll */

%token EOF
%token EOL

%token <int> INT       /* le lex�me INT a un attribut entier */
%token PLUS TIMES MOINS DIV
%token EGAL NONEGAL INF_S INF_L SUP_S SUP_L
%token LPAREN RPAREN
%token LET IN
%token FUN DONNE
%token IF THEN ELSE
%token <string> NOM


%left PLUS   /* associativit� gauche: a+b+c, c'est (a+b)+c */
%left MOINS
%left TIMES
%left DIV
/*aucune idee de l'associativite des let*/


%start main             /* "start" signale le point d'entr�e: */
                        /* c'est ici main, qui est d�fini plus bas */
%type <Expr.expr> main     /* on _doit_ donner le type associ� � main, qui est le point d'entr�e */

%%
    /* --- d�but des r�gles de grammaire --- */
                            /* � droite, les valeurs associ�es */


main:                       
    expression EOF               { $1 } 
;
expression:			    /* r�gles de grammaire pour les expressions */
  | INT                                           { Const $1 }
  | LPAREN expression RPAREN                      { $2 }

  
  | expression PLUS expression                    { Add($1,$3) }
  | expression TIMES expression                   { Mul($1,$3) }
  | expression MOINS expression                   { Sou($1,$3) }
  | MOINS expression                              { Sou( Const(0), $2) }
  | expression DIV expression                     { Div($1,$3) }

  
  | LET NOM EGAL expression IN expression         { Let($2, $4, $6) }
  | IF expression THEN expression ELSE expression { Cond($2, $4, $6) } /* Attention $ pas S :p */

  | expression EGAL expression                    { Testeq($1,$3) }
  | expression NONEGAL expression                 { Testneq($1,$3) }
  | expression INF_S expression                   { Testlt($1,$3) }
  | expression SUP_S expression                   { Testgt($1,$3) }
  | expression INF_L expression                   { Testlet($1,$3) }
  | expression SUP_L expression                   { Testget($1,$3) }

  /* Errors Managements */

;

