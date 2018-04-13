{
  open Errmgr
  open Parser;;     (* le type "token" est d�fini dans parser.mli *)
(* ce n'est pas � vous d'�crire ce fichier, il est engendr� automatiquement *)
(* exception Eof;; *)
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t']     { token lexbuf }    (* on saute les blancs et les tabulations *)
 	     	   	           (* token: appel r�cursif *)
                                   (* lexbuf: argument implicite
                                      associ� au tampon o� sont
                                      lus les caract�res *)

  |"(*" [^ '*']* "*)" { token lexbuf }
  (* File/errors management  *)
  | eof             { EOF }
  | '\n'            { Lexing.new_line lexbuf; token lexbuf }

  (* Built in *)
  |"prInt"	    { PRINT }


  (* Arith *)

  | '+'             { PLUS }
  | '*'             { TIMES }
  | '-'             { MOINS }
  | '/'             { DIV }
  | '('             { LPAREN }
  | ')'             { RPAREN }

    (*mots-cl�s*)
  | "let"           { LET }
  | "rec"	    { REC }
  | "in"            { IN }
  | ';'		    { SEMICOL }
  | ";;"	    { DOUBLESEMICOL }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }

  | "match" 	    { MATCH }
  | "with" 	    { WITH }
  | "function"	    { FUNCTION }

  | ','             { COMMA }

  | '['             { LBRACKET }
  | ']'             { RBRACKET }
  | "::"            { COLONCOLON }

  | "fun"           { FUN }
  | "->"            { DONNE }
  | "|"		    { CASE }
  | "()"            { UNIT }


  (*aspects imp�ratifs*)
  | "ref"           { REF }
  | ":="            { AFF }
  | '!'             { BANG }

  (*op�rateurs logiques*)
  | '<'             { INF_S }
  | '>'             { SUP_S }
  | "<="            { INF_L }
  | ">="            { SUP_L }
  | '='             { EGAL }
  | "<>"            { NONEGAL }

  (*gestion des exceptions*)
  | "try"           { TRY }
  | "raise"         { RAISE }

  (*strings and litterals*)
  | ['0'-'9']+ as s { INT (int_of_string s) }
  | ['a'-'z' '_'] ['a'-'z' '0'-'9' 'A'-'Z' '_']* as s { NOM (s) }
  | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as s { CONSTR (s) }
