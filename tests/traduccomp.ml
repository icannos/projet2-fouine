let e1 x= x in
let e2 x= x in
let e3 x= x in
let e4 x= x in
(fun s0 -> let (b1, s1) = e1 s1 in
           let (b2, s2) = e2 s2 in
               if b1 = b2 then e3 s2 else e4 s2);;
