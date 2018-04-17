

let f = ref fun x -> 3*x in let g = ref f in let _ = prInt (! !g 3) in 
let a = ref 5 in let setx_and_get_ref x a = (a := x; a) in prInt !(setx_and_get_ref 10 a)
