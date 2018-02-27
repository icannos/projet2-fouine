
open Arguments

(* Some shortcuts *)
let ps = print_string;;

(* Conditinnal prints *)

let p_debug s = if !debugmode then print_string s else ();;
let p_verbose s = if !verbosemode then print_string s else ();;

let prInt x = print_int x;print_newline(); x;;
