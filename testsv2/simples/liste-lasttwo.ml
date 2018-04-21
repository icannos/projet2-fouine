let rec last_two l = match l with
    | [x;y] -> (x,y)
    | x::t -> last_two t
    in

    let (x,y) = last_two ([1;2;3;4;5;6]) in prInt x; prInt y
