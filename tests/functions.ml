let c = 8 in
let f = (fun x -> c*x) in
let c = 5 in
let _ = prInt (f 5) in
 prInt ((fun x -> 2*x) 3)
