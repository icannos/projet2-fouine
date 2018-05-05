let printint x = prInt x;;

let rec liste_iter f l = match l with
|[] -> ()
|x::q -> f x; liste_iter f q
;;

let _ = liste_iter printint [1;2;3;4;56;7;8];;

let square x = x*x;;

let rec liste_map f l = match l with
| [] -> []
| x::q -> (f x)::(liste_map f q)
;;

liste_iter printint (liste_map square [1;2;3;4;56;7;8])
