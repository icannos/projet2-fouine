(** Gère l'ensemble des arguments pouvant être passés en ligne de commande à fouine*)

open Printf

(* Gestion Globale des arguments passés à l'exécutable *)

(** Modes de débuggages *)

let debugmode = ref false;;
let verbosemode = ref false;;
let mem_mode = ref false;;

(** Modes de traduction*)
let tradimp = ref false;;
let tradexcep = ref false;;
let impexcep = ref false;;
let excepimp = ref false;;

let outcode = ref false;;

(**pour la compilation*)
let stackcode = ref false;;
let machine = ref false;;

(** typechecking *)

let typecheckingmode = ref false;;
let displaytype = ref false;;


(** Fichier contenant les fonctions de mémoire *)
let mem_file = ref ((Filename.dirname (Sys.argv.(0))) ^ "/memfonc.ml");;

(* On a une chaîne vide originnellement *)
let srcfile = ref "";;

(** Lit un in_channel entier jusqu'à tomber sur End_of_file *)
let read_all chan =let data = ref "" in
try
  while true; do
    data := " \n"^ !data ^ (input_line chan) ^ " \n"
  done; !data
  with End_of_file -> close_in chan; !data
;;

(** Lit spécifiquement un fichier *)

let read_file filename = let chan = open_in filename in read_all chan;;

(** Lit le fichier source passé en argument anonyme et le copie sous forme de string dans srcfile *)
let getsrcfile filename = srcfile := (!srcfile) ^ (read_file filename);;

let set_mem filename = mem_file := filename;;

(** Ensemble des arguments possibles*)
let optlist =
  [
    ("-debug", Arg.Set debugmode, "Active le mode de debuggage" );
    ("--debug", Arg.Set debugmode, "Active le mode de debuggage" );
    ("-d", Arg.Set debugmode, "Active le mode de debuggage" );
    ("--verbose", Arg.Set verbosemode, "Active le mode verbose" );
    ("-v", Arg.Set verbosemode, "Active le mode verbose" );
    ("-m", Arg.Set mem_mode, "Affiche le dernier environnement connu");
    ("-R", Arg.Set tradimp, "Active la traduction des aspects imperatifs");
    ("--memory-file", Arg.String set_mem, "Permet de spécifier le fichier dans lequel aller chercher les fonctions de gestion de la mémoire");
    ("-E", Arg.Set tradexcep, "Active la traduction en continuation");
    ("-RE", Arg.Set excepimp, "Active la traduction en continuation puis en supprimant les aspects imperatifs");
    ("-ER", Arg.Set impexcep, "Active la traduction des aspects imperatifs suivie de la reecriture en continuations");
    ("-machine", Arg.Set machine, "Active l'execution du code compile");
    ("-stackcode", Arg.Set stackcode, "Active l'affichage du code compile");
    ("-outcode", Arg.Set outcode, "Affiche le programme traduit à l'écran sans l'exécuter");

    ("-disptype", Arg.Set displaytype, "Affiche le type d'une expression sans exécuter le code");
    ("-typecheck", Arg.Set displaytype, "Active la vérification de type avant l'exécution")
  ]
;;

(** Message de doc  *)
let usage = "Interpreter Fouine -- Edwige Cyffers & Maxime Darrin";;
