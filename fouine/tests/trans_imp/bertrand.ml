let x = ref 5 in
 let y = ref x in
 let _ = x := !x * 2 in
 prInt (!x + !(!y)) 
