

(* Will store the current line for error reports *)
let line_number = ref 0;;

(* increment line number  *)
let incr_line () = line_number := !line_number + 1;;

