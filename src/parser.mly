%{
(* --- pr�ambule: ici du code Caml --- *)

open Expr
open Errmgr

%}
/* description des lex�mes, ceux-ci sont d�crits dans lexer.mll */

%token EOF
%token EOL

%token <int> INT       /* le lex�me INT a un attribut entier */
%token PLUS TIMES MOINS DIV
%token EGAL  NONEGAL INF_S INF_L SUP_S SUP_L
%token LPAREN RPAREN SEMICOL DOUBLESEMICOL
%token LET IN REC
%token FUN DONNE CASE MATCH WITH FUNCTION
%token <string> CONSTR
%token IF THEN ELSE
%token <string> NOM
%token PRINT
%token REF BANG AFF
%token COMMA
%token UNIT


%nonassoc LPAREN
%nonassoc RPAREN

%right IN
%right LET
%right CASE

%right CONSTR
%right COMMA

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

%left PLUS   /* associativit� gauche: a+b+c, c'est (a+b)+c */
%left MOINS
%left PRINT
%left TIMES
%left DIV




%start main

%type <Expr.extexpr> main

%%

main:
  |toplevel EOF					{ $1 }
  |simplexpr EOF               			{ $1 }
;


toplevel:   /* les let de surface */
  |binding toplevel				{  (error_handler  (), Let($1, $2)) }   /* le cas de base let a = 5*/
  |binding DOUBLESEMICOL toplevel 		{  (error_handler  (), Let($1, $3)) }   /* let a = 5 ... let b = 8;; expr*/
  |binding DOUBLESEMICOL simplexpr		{  (error_handler  (), Let($1, $3)) }   /* let a = 5 ... let v = 2;; a*v */
  |binding IN toplevel	   			{  (error_handler  (), Let($1, $3)) }   /* let a = 5 in 2+2 */

  |binding    					{  (error_handler  (),
  						   Let($1, (error_handler  (), Const 0)))  }

  |recursive IN toplevel			{  (error_handler  (), LetRec($1, $3)) }
  |recursive DOUBLESEMICOL toplevel		{  (error_handler  (), LetRec($1, $3)) }
  |recursive DOUBLESEMICOL simplexpr		{  (error_handler  (), LetRec($1, $3)) }
  |recursive toplevel	     			{  (error_handler  (), LetRec($1, $2)) }
  |recursive 					{  (error_handler  (),
  						   LetRec($1, (error_handler  (),Const 0)))}

;

binding:
  |LET pattern functexpr                	{($2, $3)} /*on renvoie un couple (identifier, expression)*/
;

pattern: /* c'est les différentes choses qu'on peut matcher */
  | INT 					{  (error_handler (),  Const $1 ) }
  | LPAREN cartesian_prod RPAREN	      	{  (error_handler  (), Cart $2)}
  | CONSTR LPAREN cartesian_prod RPAREN         {  (error_handler  (), Constr($1, $3)) }
  | NOM                                      	{  (error_handler  (), Identifier $1 ) }

;

cartesian_prod:   /*les n uplet, rangés dans une liste dans un Cart*/
|pattern					{   [$1] }
|pattern COMMA cartesian_prod			{   $1::$3 }
;

pattern_case:     /*dans une fonction les differents cas possibles  */ 
|pattern DONNE simplexpr			{ (error_handler (), PattCase($1, $3)) }
;

pattern_listcases: /*les différents matching sont rangés dans une liste : l'ordre importe  */
| CASE pattern_case					{ [$2] }
| CASE pattern_case pattern_listcases			{ $2::$3 }

;
recursive:
  |LET REC NOM functexpr			{($3, $4)}

;

functexpr:
  | EGAL simplexpr				{ $2 }
  | UNIT functexpr				{  (error_handler  (), Fun("_", $2)) }
  | NOM functexpr                       	{  (error_handler  (), Fun($1, $2)) }
;

cart_expr:
|simplexpr COMMA simplexpr		       	{ [$1;$3] }
|simplexpr COMMA cart_expr  			{ ($1::$3) }

;

constr_expr:
|simplexpr		       	{ [$1] }
|simplexpr COMMA cart_expr  			{ ($1::$3) }

;

simplexpr:
  | PRINT simplexpr				{  (error_handler  (),PrintInt $2) }
  | FUN NOM DONNE simplexpr                     {  (error_handler  (),Fun($2, $4)) }
  | simplexpr PLUS simplexpr                    {  (error_handler  (),Add($1,$3)) }
  | simplexpr TIMES simplexpr                   {  (error_handler  (),Mul($1,$3)) }
  | simplexpr MOINS simplexpr                   {  (error_handler  (),Sou($1,$3)) }
  | simplexpr DIV simplexpr                     {  (error_handler  (),Div($1,$3)) }
  | MOINS simplexpr                             {  (error_handler  (),Sou((error_handler  (),Const(0)), $2)) }
  | binding IN simplexpr			{  (error_handler  (),Let($1, $3)) }
  | recursive IN simplexpr			{  (error_handler  (), LetRec($1, $3)) }
  | IF bexpr THEN simplexpr ELSE simplexpr      {  (error_handler  (),Cond($2, $4, $6)) }
  | 
  | listexpr                                    { $1 }

  | simplexpr SEMICOL simplexpr                 {  (error_handler  (), Let(((error_handler  (),Identifier "_"),$1),$3)) }
  | NOM AFF simplexpr 				{  (error_handler  (),Aff($1, $3)) }
  | REF simplexpr				{  (error_handler  (),Ref($2)) }

  | CONSTR LPAREN constr_expr RPAREN				{  (error_handler  (), Constr($1, $3))}
  | LPAREN cart_expr RPAREN 					{  (error_handler  (), Cart $2 ) }
  
  | MATCH simplexpr WITH pattern_listcases			{  (error_handler (), Match($2, $4) )}

  |UNIT	  	    	 					{(error_handler (), Uni)}


;


listexpr:
  | priexpr                                     { $1 }
  | listexpr priexpr                            {  (error_handler  (), App($1, $2)) }
;


priexpr:
  | NOM	      	     		       	   	{  (error_handler  (), Identifier $1) }
  | BANG NOM					{  (error_handler  (), Acc $2 )    }
  | INT                                         {  (error_handler  (), Const $1) }
  | LPAREN simplexpr RPAREN                     { $2 }
;

bexpr:
  | simplexpr EGAL simplexpr                    {  (error_handler  (),Testeq($1,$3)) }
  | simplexpr NONEGAL simplexpr                 {  (error_handler  (),Testneq($1,$3)) }
  | simplexpr INF_S simplexpr                   {  (error_handler  (),Testlt($1,$3)) }
  | simplexpr SUP_S simplexpr                   {  (error_handler  (),Testgt($1,$3)) }
  | simplexpr INF_L simplexpr                   {  (error_handler  (),Testlet($1,$3)) }
  | simplexpr SUP_L simplexpr                   {  (error_handler  (),Testget($1,$3)) }

;
