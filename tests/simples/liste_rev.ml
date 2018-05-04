let rec print_list l = match l with
|[] -> ()
|x::q -> prInt x; print_list q
;;

let rec rev aux l = match l with
|[] -> aux
|[x] -> x::aux
|x::q -> rev (x::aux) q
;;

print_list (rev [] [1;2;3;4;56;7;8])
