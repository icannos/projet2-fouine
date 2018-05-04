let c = 8 in
let f = fun x -> fun y -> fun z -> y+c*x-z in
let _ = prInt (f 5 10 6) in 0 
