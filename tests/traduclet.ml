let e1 x= x in
let e2 x= x in
    (fun s -> let (x,s1) = e1 s in e2 s1);;
