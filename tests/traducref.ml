fun s -> let (v, s1) = e s in let (l,s2) = allocate v s1 in (l,s2)
