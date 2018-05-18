(** Quelques fonctions utilitaires utiles tout au long du projet, sans être spécifiques à fouine*)
open Arguments

(* Some shortcuts *)
let ps = print_string;;

(* Conditinnal prints *)

let p_debug s = if !debugmode then print_string s else ();;
let p_verbose s = if !verbosemode then print_string s else ();;

let prInt x = print_int x;print_newline(); x;;

(*Ajout d'une fonction pour mettre des pointvirgules dans les n-uplets, je trouve ça plus claire que ta méthode, à trancher*)
let rec join sep liste = match liste with
  | [] -> ""
  | [a] -> a
  | a::q -> a ^ sep ^ join sep q;;

let rec tab n = match n with
  |0-> ""
  |n-> "  " ^ tab (n-1);;
