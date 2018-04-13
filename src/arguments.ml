open Printf

(* Gestion Globale des arguments passés à l'exécutable *)

(* Modes de débuggages *)

let debugmode = ref false;;
let verbosemode = ref false;;

(*Modes de traduction*)
let tradimp = ref false;;
let tradexcep = ref false;;

(* Fichier contenant les fonctions de mémoire *)
let mem_file = ref ((Filename.dirname (Sys.argv.(0))) ^ "/memfonc.ml");;

(* On a une chaîne vide originnellement *)
let srcfile = ref "";;

(* Lit un in_channel entier jusqu'à tomber sur End_of_file *)
let read_all chan =let data = ref "" in
try
  while true; do
    data := !data ^ input_line chan
  done; !data
  with End_of_file -> close_in chan; !data
;;

(* Lit spécifiquement un fichier *)

let read_file filename = let chan = open_in filename in read_all chan;;

(* Lis le fichier source passé en argument anonyme et le copie sous forme de string dans srcfile *)
let getsrcfile filename = srcfile := read_file filename;;

let set_mem filename = mem_file := filename;;


let optlist =
  [
    ("-debug", Arg.Set debugmode, "Active le mode de debuggage" );
    ("--debug", Arg.Set debugmode, "Active le mode de debuggage" );
    ("-d", Arg.Set debugmode, "Active le mode de debuggage" );
    ("--verbose", Arg.Set verbosemode, "Active le mode verbose" );
    ("-v", Arg.Set verbosemode, "Active le mode verbose" );
    ("-R", Arg.Set tradimp, "Active la traduction des aspects imperatif");
    ("--memory-file", Arg.String set_mem, "Permet de spécifier le fichier dans
    lequel aller chercher les fonctions de gestion de la mémoire")
  ]
;;

(* Message de doc  *)
let usage = "Interpreter Fouine -- Edwige Cyffers & Maxime Darrin";;
