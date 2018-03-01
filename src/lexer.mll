{
  open Errmgr;;
  open Parser;;     (* le type "token" est défini dans parser.mli *)
(* ce n'est pas à vous d'écrire ce fichier, il est engendré automatiquement *)
(* exception Eof;; *)
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n']     { token lexbuf }    (* on saute les blancs et les tabulations *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)


  (* File/errors management  *)
  | eof             { EOF }
  | '\n'            { EOL }

  (* Built in *)
  |"prInt"	    { PRINT }
  

  (* Arith *)

  | '+'             { PLUS }
  | '*'             { TIMES }
  | '-'             { MOINS }
  | '/'             { DIV }
  | '('             { LPAREN }
  | ')'             { RPAREN }

    (*mots-clés*)
  | "let"           { LET }  (*j'ai mis trois plombes à comprendre qu'il fallait des guillements*)
  | "in"            { IN }
  | ";;"	    { IN }
  | ';'		    { SEMICOL }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }

  | "fun"           { FUN }
  | "->"            { DONNE }

  (*opérateurs logiques*)
  | '<'             { INF_S }
  | '>'             { SUP_S }
  | "<="            { INF_L }
  | ">="            { SUP_L }
  | '='             { EGAL }
  | "<>"            { NONEGAL }
  | ['0'-'9']+ as s { INT (int_of_string s) }
  | ['a'-'z' '_']+ as s { NOM (s) }
  
