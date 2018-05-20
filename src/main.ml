(** Intrepeter fouine*)

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
open Showmachine;;
open Typechecking

(** Parse une chaine de caractère à l'aide d'ocamlyacc *)
let parse_string s =
  let lexbuf = Lexing.from_string s in
  Parser.main Lexer.token lexbuf
;;

(** Initialise l'environnement au début de l'exécution de l'interpreter,
elle cela pourrait servir à l'implémentation d'un interpreter intéractif *)
let initialize_envir () =
  match (!tradimp || !impexcep || !excepimp) with
  | true ->
  let lexbuf = Lexing.from_string (read_file !mem_file) in
      let parse () = Parser.main Lexer.token lexbuf in
          let _ = eval (parse ()) (Environnement.empty) in !toplevel_envir
  | false -> Environnement.empty

;;

(** Fonction principale: c'est le main de notre programme*)
let interpreter () =

  (* On parse les arguments passés en CLI *)
  (* Arg.parse List -> (anon_arg string -> ()) -> in_channel*)
  Arg.parse optlist getsrcfile usage;

 (* On  initialise le parseur et le lexeur en lui donnant une string contenant
 la concaténation de tous les fichiers spécifiés en entrée ainsi que stdin

  Voir Arguments.ml pour les déclarations.
*)

  let memory_functions = begin
  match (!tradimp || !impexcep || !excepimp) with
    | true -> (read_file !mem_file)
    | false -> ""
    end
    in
    let lexbuf = Lexing.from_string (memory_functions ^ " \n " ^ !srcfile) in


   let parse () = Parser.main Lexer.token lexbuf in
   let ast = parse () in if !verbosemode then (print_string (istring_of_expr ast));
                         (*Les différentes traductions possibles*)
    let ast = if !tradimp then exec_trad ast else  ast   in
    let ast = if !tradexcep then exec_excep ast else ast in
    let ast = if !excepimp then exec_trad (exec_excep ast) else ast in
    let ast = if !impexcep then exec_excep (exec_trad ast) else ast in

    if(!debugmode) then (aff_expr ast; print_newline());
    if(!outcode) then (aff_expr ast; print_newline());

    if(!displaytype) then(
    let (t, env) = infer ast (EnvType.empty) (EnvType.empty) in
    print_string ("Type de sortie: " ^ (string_of_ftype EnvType.empty t) ^ "\n"));

    (if(!typecheckingmode) then (let _ = infer ast (EnvType.empty) (EnvType.empty) in ()));

    if !stackcode then (affiche_code (compile ast));
    if !machine then (execution (compile ast));
  let _ = if(!outcode = false && !displaytype == false && !machine == false && !stackcode == false) then
  eval ast (Environnement.empty ) else Int 0 in

  if(!mem_mode) then (print_env !toplevel_envir) else ();

  flush stdout;
;;


(* On  exécute l'interpréteur *)
let _ = interpreter ();;
