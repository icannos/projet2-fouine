let rec last_two l = match l with
    | [] -> raise NotLongEnough
    | [x] -> raise NotLongEnough
    | [x;y] -> (x,y)
    | x::t -> (last_two t)
in


(try let a = 0 in (try last_two [2;3]; if a = 0 then raise Division_by_zero else 8/a with Division_by_zero -> prInt 1)
with NotLongEnough -> prInt 0
);

(try let a = 1 in (try last_two [2]; if a = 0 then raise Division_by_zero else 8/a with Division_by_zero -> prInt 1)
with NotLongEnough -> prInt 0
)
