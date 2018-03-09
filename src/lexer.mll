{
  open Errmgr
  open Parser;;     (* le type "token" est défini dans parser.mli *)
(* ce n'est pas à vous d'écrire ce fichier, il est engendré automatiquement *)
(* exception Eof;; *)
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t']     { token lexbuf }    (* on saute les blancs et les tabulations *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)


  (* File/errors management  *)
  | eof             { EOF }
  | '\n'            { incr_line (); token lexbuf }
  | "test"	    { TEST }
  
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
  | "let"           { LET }
  | "rec"	    { REC }
  | "in"            { IN }
  | ';'		    { SEMICOL }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }

  | "fun"           { FUN }
  | "->"            { DONNE }

  (*aspects impératifs*)
  | "ref"           { REF }
  | ":="            { AFF }
  | '!'             { BANG }

  (*opérateurs logiques*)
  | '<'             { INF_S }
  | '>'             { SUP_S }
  | "<="            { INF_L }
  | ">="            { SUP_L }
  | '='             { EGAL }
  | "<>"            { NONEGAL }
  | ['0'-'9']+ as s { INT (int_of_string s) }
  | ['a'-'z' '_']+ as s { NOM (s) }
  
