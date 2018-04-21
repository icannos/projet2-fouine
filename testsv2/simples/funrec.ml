let rec f x  =  (if x = 1 then 1 else 1 + (f (x-1))) in let _ = prInt (f 5) in 0
