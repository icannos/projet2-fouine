open Tradimp;;
open Tradcont;;
open Arguments;;
open Expr;;
open Env;;
open Safe;;
open Parser;;
open Affichage;;
open Eval;;
open Tradmachine;;

let parse_string s =
  let lexbuf = Lexing.from_string s in
  Parser.main Lexer.token lexbuf
;;

let initialize_envir () =
  match (!tradimp || !impexcep || !excepimp) with
  | true ->
  let lexbuf = Lexing.from_string (read_file !mem_file) in
      let parse () = Parser.main Lexer.token lexbuf in
          let _ = eval (parse ()) (Environnement.empty) in !toplevel_envir
  | false -> Environnement.empty

;;

(* Fonction principale  *)
let interpreter () =

  (* On parse les arguments passés en CLI *)
  (* Arg.parse List -> (anon_arg string -> ()) -> in_channel*)
  Arg.parse optlist getsrcfile usage;

 (* On  initialise le parseur et le lexeur en lui donnant une string contenant
 la concaténation de tous les fichiers spécifiés en entrée ainsi que stdin

  Voir Arguments.ml pour les déclarations.
*)


  let lexbuf = Lexing.from_string (!srcfile) in
  

   let parse () = Parser.main Lexer.token lexbuf in
   let ast = parse () in if !verbosemode then (print_string (istring_of_expr ast));
                         (*Les différentes traductions possibles*)
    let ast = if !tradimp then exec_trad ast else  ast   in
    let ast = if !tradexcep then exec_excep ast else ast in
    let ast = if !excepimp then exec_trad (exec_excep ast) else ast in
    let ast = if !impexcep then exec_excep (exec_trad ast) else ast in

    if(!debugmode) then (aff_expr ast; print_newline());
    if(!outcode) then (aff_expr ast; print_newline());

    if !stackcode then (affiche_code (compile ast));
    if !machine then (execution (compile ast));
  let _ = if(!outcode = false) then eval ast (initialize_envir ()) else Int 0 in

  if(!mem_mode) then (print_env !toplevel_envir) else ();

  flush stdout;
;;


(* On  exécute l'interpréteur *)
let _ = interpreter ();;
