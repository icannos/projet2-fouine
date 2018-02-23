open Printf
   
(* Gestion Globale des arguments passés à l'exécutable *)

(* Modes de débuggages *)

let debugmode = ref false;;
let verbosemode = ref false;;


(* Par défaut on initialise sur le flux standard, on le remplacera ensuite par le fichier que l'on aura ouvert *)
let srcfile = ref stdin;; (* Type in_channel *)

let getsrcfile filename =
  (* Lis le fichier source passé en argument anonyme et l'ouvre comme flux *)
  srcfile := open_in filename
;;

(* Arguments disponibles  *)

let optlist =
  [
    ("-debug", Arg.Set debugmode, "Active le mode de debuggage" );
    ("--debug", Arg.Set debugmode, "Active le mode de debuggage" );
    ("-d", Arg.Set debugmode, "Active le mode de debuggage" );
    ("--verbose", Arg.Set verbosemode, "Active le mode verbose" );
    ("-v", Arg.Set verbosemode, "Active le mode verbose" );
  ]
;;

(* Message de doc  *)
let usage = "Interpreter Fouine -- Edwige Cyffers & Maxime Darrin";;


