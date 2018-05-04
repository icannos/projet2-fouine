let rec last_two l = match l with
    | [] -> raise NotLongEnough
    | [x] -> raise NotLongEnough
    | [x;y] -> (x,y)
    | x::t -> last_two t
;;


try last_two [2] with NotLongEnough -> prInt 0
