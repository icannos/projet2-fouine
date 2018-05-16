fun x -> (if x < 3 then (fun x y -> y x) else (fun x y -> y x))
