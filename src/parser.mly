%{
(* --- pr�ambule: ici du code Caml --- *)

open Expr
open Errmgr

%}
/* description des lex�mes, ceux-ci sont d�crits dans lexer.mll */

%token EOF

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
%token TYPE OF
%token COMMA
%token UNIT
%token LBRACKET RBRACKET COLONCOLON COLON
%token TRY RAISE


%right CONSTR


%nonassoc LPAREN
%left RPAREN
%nonassoc LBRACKET
%left RBRACKET

%left DOUBLESEMICOL
%right IN

%right LET
%right REC
%right CASE
%right NOM



%right COMMA
%right COLONCOLON

%left EGAL

%left REF
%left NONEGAL
%left INF_S
%left INF_L
%left SUP_S
%left SUP_L

%right IF
%right THEN
%right ELSE
%right DONNE

%left SEMICOL
%left RAISE
%left AFF

%left PLUS   /* associativit� gauche: a+b+c, c'est (a+b)+c */
%left MOINS
%left TIMES
%left DIV
%left PRINT

%start main

%type <Expr.extexpr> main

%%

main:
 |toplevel EOF					{ $1 }
 |simplexpr EOF               			{ $1 }
;


idtyped:
  |NOM {(error_handler  (), Identifier($1, (0,Typed((error_handler  (), TypeId "_")))))}
  |LPAREN NOM COLON typed RPAREN  {(error_handler  (), Identifier($2, (0,Typed $4)))}
;

typed:
|NOM {(error_handler  (), TypeId $1 )}
|typed DONNE typed {(error_handler  (),  Fun($1, $3) )}
|LPAREN prod_cart RPAREN {(error_handler  (),  Cart($2) )}

;

t_sum:
|CONSTR                                      {  (error_handler  (), Constr($1, [])) }
|CONSTR OF prod_cart                         {  (error_handler  (), Constr($1, $3)) }
;

t_cases:
|t_sum                                        {[$1]}
|t_sum CASE t_cases                           {$1::$3}
;

toplevel:   /* les let de surface */
 |binding toplevel				{  (error_handler  (), Let($1, $2)) }   /* le cas de base let a = 5 in expr*/
 |binding DOUBLESEMICOL toplevel 		{  (error_handler  (), Let($1, $3)) }   /* let a = 5 ... let b = 8;; expr*/
 |binding DOUBLESEMICOL simplexpr		{  (error_handler  (), Let($1, $3)) }   /* let a = 5 ... let v = 2;; a*v */
 |binding IN toplevel	   			{  (error_handler  (), Let($1, $3)) }   /* let a = 5 in 2+2 */
 |binding    					{  (error_handler  (),
        Let($1, (error_handler  (), Const 0)))  }  /*le cas let a = 2 traduit en let a = 2 in 0*/
 |binding DOUBLESEMICOL					{  (error_handler  (),
               Let($1, (error_handler  (), Const 0)))  }

 |rec_binding IN toplevel			{  (error_handler  (), LetRec($1, $3)) } /*idem avec les let rec */
 |rec_binding DOUBLESEMICOL toplevel		{  (error_handler  (), LetRec($1, $3)) }
 |rec_binding DOUBLESEMICOL simplexpr		{  (error_handler  (), LetRec($1, $3)) }
 |rec_binding toplevel	     			{  (error_handler  (), LetRec($1, $2)) }
 |rec_binding 					{  (error_handler  (), LetRec($1, (error_handler  (),Const 0)))}
 |rec_binding DOUBLESEMICOL			{  (error_handler  (), LetRec($1, (error_handler  (), Const 0)))  }

 /* Type declaration */
 |TYPE NOM EGAL t_cases DOUBLESEMICOL toplevel              {(error_handler (), RecordType(($2, $4), $6))}
 |TYPE NOM EGAL typed DOUBLESEMICOL simplexpr            {(error_handler (), RecordType(($2, [$4]), $6))}
 |TYPE NOM EGAL t_cases DOUBLESEMICOL              {(error_handler (), RecordType(($2, $4), (error_handler  (), Const 0) ))}
 |TYPE NOM EGAL typed DOUBLESEMICOL            {(error_handler (), RecordType(($2, [$4]), (error_handler  (), Const 0)))}
 ;

 prod_cart:
 |typed {[$1]}
 |typed TIMES prod_cart {$1::$3}
 ;


simplexpr:
 | PRINT simplexpr			       {  (error_handler  (),PrintInt $2) }
 | FUN funexpr                     {  $2 }
 | simplexpr PLUS simplexpr                    {  (error_handler  (),Add($1,$3)) }
 | simplexpr TIMES simplexpr                   {  (error_handler  (),Mul($1,$3)) }
 | simplexpr MOINS simplexpr                   {  (error_handler  (),Sou($1,$3)) }
 | simplexpr DIV simplexpr                     {  (error_handler  (),Div($1,$3)) }
 | MOINS simplexpr                             {  (error_handler  (),Sou((error_handler  (),Const(0)), $2)) }
 | binding IN simplexpr			       {  (error_handler  (),Let($1, $3)) }
 | rec_binding IN simplexpr		       {  (error_handler  (), LetRec($1, $3)) }
 | IF boolexpr THEN simplexpr ELSE simplexpr   {  (error_handler  (),Cond($2, $4, $6)) }
 | listexpr                                    { $1 }

 | simplexpr SEMICOL simplexpr                 {  (error_handler  (), Let(((error_handler  (),Identifier ("_"  , (0, Typed((0, TypeId "_"))))  ),$1),$3)) }
 | simplexpr DOUBLESEMICOL                  {  (error_handler  (), Let(((error_handler  (),Identifier ("_"  , (0, Typed((0, TypeId "_"))))),$1),(error_handler  (),Const 0))) }
 | priexpr AFF simplexpr 			       {  (error_handler  (), Aff($1, $3)) }
 | REF simplexpr			       {  (error_handler  (),Ref($2)) }
 | UNIT	  	    	 		       {(error_handler (), Uni)}

  | LPAREN n_uplets RPAREN 		       {  (error_handler  (), Cart $2 ) }
  | liste                                       { $1 }

 | CONSTR LPAREN uplet_simplexpr RPAREN	       {  (error_handler  (), Constr($1, $3))}

 | CONSTR NOM                             	       {  (error_handler  (), Constr($1, [(error_handler  (), Identifier($2, (0,Typed((error_handler  (), TypeId "_")))))]))}
 | CONSTR INT                             	       {  (error_handler  (), Constr($1, [(error_handler  (), Const $2)]))}
 | CONSTR                             	       {  (error_handler  (), Constr($1, []))}

 | MATCH simplexpr WITH list_pattern_case      {  (error_handler (), Match($2, $4) )}
 | TRY simplexpr WITH list_pattern_case        {  (error_handler (), Try($2, $4)) }
 | MATCH simplexpr WITH lonely_pattern      {  (error_handler (), Match($2, $4) )}
 | TRY simplexpr WITH lonely_pattern        {  (error_handler (), Try($2, $4)) }
 | RAISE simplexpr                             {  (error_handler (), Raise($2)) }
;


binding:
 |LET pattern functexpr                	{($2, $3)} /*on renvoie un couple (identifier, expression)*/
;

rec_binding:
 |LET REC idtyped functexpr			{($3, $4)}
;

functexpr:
 | EGAL simplexpr				{ $2 }
 | UNIT functexpr				{  (error_handler  (), Fun((error_handler () ,Identifier ("_",(0, Typed((0, TypeId "_"))))), $2)) }
 | pattern functexpr                        	{  (error_handler  (), Fun($1, $2)) }
;

funexpr:
 | DONNE simplexpr				{ $2 }
 | UNIT funexpr				{  (error_handler  (), Fun((error_handler () ,Identifier ("_",(0, Typed((0, TypeId "_"))))), $2)) }
 | pattern funexpr                        	{  (error_handler  (), Fun($1, $2)) }
;

pattern: /* c'est les différentes choses qu'on peut matcher */
 | INT 					        {  (error_handler (),  Const $1 ) }
 | LPAREN uplet_pattern RPAREN	        	{  (error_handler  (), Cart $2)}
 | CONSTR idtyped          {  (error_handler  (), Constr($1, [$2])) }
 | CONSTR LPAREN uplet_pattern RPAREN           {  (error_handler  (), Constr($1, $3)) }
 | CONSTR                                      {  (error_handler  (), Constr($1, [])) }
 | idtyped                                      	{   $1  }
 | liste_pattern                                {  $1 }


pattern_case:     /*dans une fonction les differents cas possibles  */
 |pattern DONNE simplexpr			{ (error_handler (), PattCase($1, $3)) }
;

list_pattern_case: /*les différents matching sont rangés dans une liste : l'ordre importe  */
 | CASE pattern_case					{ [$2] }
 | CASE pattern_case list_pattern_case			{ $2::$3 }
;

lonely_pattern:
| pattern_case					{ [$1] }
;

uplet_pattern:   /*les n uplet, rangés dans une liste dans un Cart*/
 |pattern					{   [$1] }
 |pattern COMMA uplet_pattern			{   $1::$3 }
;

uplet_simplexpr:
 |simplexpr		                	{ [$1] }
 |simplexpr COMMA uplet_simplexpr  			{ ($1::$3) }
;

n_uplets:  /*on a au moins deux éléments*/
 |simplexpr COMMA simplexpr		       	{ [$1;$3] }
 |simplexpr COMMA n_uplets  			{ ($1::$3) }
;

liste_pattern:
  |LBRACKET RBRACKET                             {(error_handler (),  Vide ) }
  |LBRACKET interior_liste RBRACKET              { $2 }
  |pattern COLONCOLON pattern              { (error_handler (), Liste($1,$3)) }
;



liste:
  |LBRACKET RBRACKET                             { (error_handler (), Vide) }
  |LBRACKET interior_liste RBRACKET              { $2 }
  |priexpr COLONCOLON priexpr                    { (error_handler (), Liste($1, $3)) }
  |priexpr COLONCOLON liste                      { (error_handler (), Liste($1, $3)) }
  |LPAREN n_uplets RPAREN COLONCOLON liste       { (error_handler (), Liste((error_handler  (), Cart $2 ), $5)) }
  |LPAREN n_uplets RPAREN COLONCOLON priexpr     { (error_handler (), Liste((error_handler  (), Cart $2 ), $5)) }
;

interior_liste:
  |priexpr                                     { (error_handler (), Liste($1,(error_handler (),Vide))) }
  |LPAREN n_uplets RPAREN                      { (error_handler (), Liste((error_handler  (), Cart $2 ), (error_handler (),Vide))) }
  |priexpr  SEMICOL interior_liste              { (error_handler (), Liste($1,$3)) }
  |LPAREN n_uplets RPAREN COLONCOLON interior_liste       { (error_handler (), Liste((error_handler  (), Cart $2 ), $5)) }
;

listexpr:
 | priexpr                                     { $1 }
 | listexpr LPAREN n_uplets RPAREN 		       {  (error_handler  (), App($1,(error_handler  (),Cart $3))) }
 | listexpr liste		       {  (error_handler  (), App($1,$2)) }
 | listexpr priexpr                            {  (error_handler  (), App($1, $2)) }
 | listexpr UNIT                            {  (error_handler  (), App($1,(error_handler  (),Uni) )) }
;


priexpr:
 | NOM	      	     		       	   	{  (error_handler  (), Identifier ($1,(0, Typed((0, TypeId "_"))))) }
 | BANG priexpr			        	{  (error_handler  (), Acc $2 )    }
 | INT                                          {  (error_handler  (), Const $1) }
 | LPAREN simplexpr RPAREN                      { $2 }
;

boolexpr:
 | LPAREN boolexpr RPAREN                      {  $2 }
 | simplexpr EGAL simplexpr                    {  (error_handler  (),Testeq($1,$3)) }
 | simplexpr NONEGAL simplexpr                 {  (error_handler  (),Testneq($1,$3)) }
 | simplexpr INF_S simplexpr                   {  (error_handler  (),Testlt($1,$3)) }
 | simplexpr SUP_S simplexpr                   {  (error_handler  (),Testgt($1,$3)) }
 | simplexpr INF_L simplexpr                   {  (error_handler  (),Testlet($1,$3)) }
 | simplexpr SUP_L simplexpr                   {  (error_handler  (),Testget($1,$3)) }

;
