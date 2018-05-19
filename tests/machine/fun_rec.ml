let rec f x y = if x  < 1 then  1 + f (x -1) (y+1)+y else 1 in prInt (f 5 3)
