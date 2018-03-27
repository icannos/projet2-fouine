let rec last l = match l with
    | [x] -> x
    | x::t -> last t
in
prInt (last ([1;2;3;4;56;4;8;9]))
