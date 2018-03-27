let rec at k l = match l with
    | h::t -> if k = 1 then h else (at (k-1) t)
;;

let c = (at 5 ([1;2;3;4;5;6;7;8])) in prInt c
