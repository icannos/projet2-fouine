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
%token LPAREN RPAREN SEMICOL DOUBLESEMICOL
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
                       
%type <Expr.extexpr> main    

%%

main:
  |toplevel EOF					{ $1 }
  |simplexpr EOF               			{ $1 } 
;


toplevel:   /* les let de surface */
  |binding toplevel				{  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()), Let($1, $2)) }   /* le cas de base let a = 5*/
  |binding DOUBLESEMICOL toplevel 		{  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()), Let($1, $3)) }   /* let a = 5 ... let b = 8;; expr*/
  |binding DOUBLESEMICOL simplexpr		{  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()), Let($1, $3)) }   /* let a = 5 ... let v = 2;; a*v */
  |binding IN toplevel	   			{  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()), Let($1, $3)) }   /* let a = 5 in 2+2 */

  |binding    					{  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),
  						   Let($1, (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()), Const 0)))  } 

  |recursive IN toplevel			{  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()), LetRec($1, $3)) }  
  |recursive DOUBLESEMICOL toplevel		{  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()), LetRec($1, $3)) }
  |recursive DOUBLESEMICOL simplexpr		{  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()), LetRec($1, $3)) }
  |recursive toplevel	     			{  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()), LetRec($1, $2)) }
  |recursive 					{  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),
  						   LetRec($1, (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),Const 0)))}

;

binding:
  |LET NOM functexpr				{($2, $3)} /*on renvoie un couple (identifier, expression)*/
;

recursive:
  |LET REC NOM functexpr			{($3, $4)}

;
  
functexpr:
  | EGAL simplexpr				{ $2 }
  | NOM functexpr                       	{  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()), Fun($1, $2)) }    
;	

simplexpr:
  | PRINT simplexpr				{  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),PrintInt $2) }
  | FUN NOM DONNE simplexpr                     {  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),Fun($2, $4)) }
  | simplexpr PLUS simplexpr                    {  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),Add($1,$3)) }
  | simplexpr TIMES simplexpr                   {  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),Mul($1,$3)) }
  | simplexpr MOINS simplexpr                   {  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),Sou($1,$3)) }
  | simplexpr DIV simplexpr                     {  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),Div($1,$3)) }
  | MOINS simplexpr                             {  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),Sou((error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),Const(0)), $2)) }
  | binding IN simplexpr			{  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),Let($1, $3)) }
  | recursive IN simplexpr			{  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()), LetRec($1, $3)) }
  | IF bexpr THEN simplexpr ELSE simplexpr      {  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),Cond($2, $4, $6)) }

  | listexpr                                    { $1 }

  | simplexpr SEMICOL simplexpr                 {  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),Let(("_",$1),$3)) }
  | NOM AFF simplexpr 				{  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),Aff($1, $3)) }
  | REF simplexpr				{  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),Ref($2)) }
 


;
  
listexpr:
  | priexpr                                     { $1 }
  | listexpr priexpr                            {  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),App($1, $2)) }
  
    
priexpr:
  | NOM	      	     		       	   	{  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()), Identifier $1) }
  | BANG NOM					{  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()), Acc $2 )    }
  | INT                                         {  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()), Const $1) }   
  | LPAREN simplexpr RPAREN                     { $2 }
;

bexpr:
  | simplexpr EGAL simplexpr                    {  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),Testeq($1,$3)) }
  | simplexpr NONEGAL simplexpr                 {  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),Testneq($1,$3)) }
  | simplexpr INF_S simplexpr                   {  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),Testlt($1,$3)) }
  | simplexpr SUP_S simplexpr                   {  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),Testgt($1,$3)) }
  | simplexpr INF_L simplexpr                   {  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),Testlet($1,$3)) }
  | simplexpr SUP_L simplexpr                   {  (error_handler  (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()),Testget($1,$3)) }

