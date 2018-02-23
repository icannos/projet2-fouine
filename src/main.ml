
open Arguments



(* Fonction principale  *)
let interpreter () =
  
  (* On parse les arguments passés en CLI *)
  Arg.parse optlist getsrcfile usage;

  (* On  initialise le lexeur et le lexeur en lui donnant notre fichier comme flux entrant. Voir Arguments.ml pour les déclarations *)
  let lexbuf = Lexing.from_channel (!srcfile) in
  let parse () = Parser.main Lexer.token lexbuf in

  flush stdout
;;



(* On  exécute l'interpertrer *)
let _ = interpreter ();;
