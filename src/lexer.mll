{
  open Parser;;        (* le type "token" est d�fini dans parser.mli *)
(* ce n'est pas � vous d'�crire ce fichier, il est engendr� automatiquement *)
exception Eof;;
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t']     { token lexbuf }    (* on saute les blancs et les tabulations *)
 	     	   	           (* token: appel r�cursif *)
                                   (* lexbuf: argument implicite
                                      associ� au tampon o� sont
                                      lus les caract�res *)
  | '\n'            { EOL }
  | '+'             { PLUS }
  | '*'             { TIMES }
  | '-'             { MOINS }
  | '/'             { DIV }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | "let"           { LET }  (*j'ai mis trois plombes � comprendre qu'il fallait des guillements*)
  | "in"            { IN }
  | '='             { EGAL }
  | ['0'-'9']+ as s { INT (int_of_string s) }
  | ['a'-'z']+ as s { NOM (s) }
  | eof             { raise Eof } 
