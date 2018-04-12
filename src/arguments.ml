open Printf

(* Gestion Globale des arguments passés à l'exécutable *)

(* Modes de débuggages *)

let debugmode = ref false;;
let verbosemode = ref false;;

(*Modes de traduction*)
let tradimp = ref false;;
let tradexcep = ref false;;

(* On a une chaîne vide originnellement *)
let srcfile = ref "";;

(* Lit un in_channel entier jusqu'à tomber sur End_of_file *)
let read_all chan =
try
  while true; do
    srcfile := !srcfile ^ input_line chan
  done;
  with End_of_file -> close_in chan
;;

(* Lit spécifiquement un fichier *)

let read_file filename =
  let chan = open_in filename in read_all chan
;;

(* Lis le fichier source passé en argument anonyme et le copie sous forme de string dans srcfile *)
let getsrcfile filename =
    read_file filename
;;



let optlist =
  [
    ("-debug", Arg.Set debugmode, "Active le mode de debuggage" );
    ("--debug", Arg.Set debugmode, "Active le mode de debuggage" );
    ("-d", Arg.Set debugmode, "Active le mode de debuggage" );
    ("--verbose", Arg.Set verbosemode, "Active le mode verbose" );
    ("-v", Arg.Set verbosemode, "Active le mode verbose" );
    ("-R", Arg.Set tradimp, "Active la traduction des aspects imperatif");
  ]
;;

(* Message de doc  *)
let usage = "Interpreter Fouine -- Edwige Cyffers & Maxime Darrin";;
