let trad_expr x = x in 
( fun s0 -> let (v1,s1) = trad_expr 1 s0  in
            let (v2,s2) = trad_expr 2 s1 in
                ((v1 + v2), s2)) [];;
