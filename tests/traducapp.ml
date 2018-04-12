 fun s -> let (f1, s1) = e1 s in
             let (v2, s2) = e2 s1 in
             (f1 v2 s2)
