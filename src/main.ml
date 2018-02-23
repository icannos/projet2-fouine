
open Arguments



(* Fonction principale  *)
let interpreter () =
  
  (* On parse les arguments passés en CLI *)
  Arg.parse optlist getsrcfile usage;

  (* On  initialise le lexeur et le lexeur en lui donnant notre fichier comme flux entrant *)
  let lexbuf = Lexing.from_channel !srcfile in
  let parse () = Parser.main Lexer.token lexbuf in
  
  flush stdout
;;


let _ = interpreter ();;
