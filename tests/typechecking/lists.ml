let rec f x l = if x = 0 then 0::l else x::(f (x-1) l) in f 
