open Traduction;;
open Arguments;;
open Expr;;
open Env;;
open Safe;;
open Parser;;
open Affichage;;
open Eval;;


(* Fonction principale  *)
let interpreter () =

  (* On parse les arguments passés en CLI *)
  (* Arg.parse List -> (anon_arg string -> ()) -> in_channel*)
  Arg.parse optlist getsrcfile usage;

 (* On  initialise le parseur et le lexeur en lui donnant une string contenant
 la concaténation de tous les fichiers spécifiés en entrée ainsi que stdin

  Voir Arguments.ml pour les déclarations.
*)

  if !tradimp then srcfile := (read_file !mem_file) ^ (!srcfile) else (srcfile := !srcfile);

  let lexbuf = Lexing.from_string (!srcfile) in



   let parse () = Parser.main Lexer.token lexbuf in
    let ast = parse ()

    in


  if(!debugmode) then (aff_expr ast; print_newline());
  let _ = eval ast ( Environnement.empty) in

  flush stdout;
;;


(* On  exécute l'interpréteur *)
let _ = interpreter ();;
