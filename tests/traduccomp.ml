let e1 x= x in
let e2 x= x in
let b x= x in
       (fun s0 -> let (b1, s1) = b s0 in
               if b1=0 then e1 s1 else e2 s1);;
