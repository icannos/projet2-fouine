let b = ref 0 in
let c = ref b in
let d = ref c in

let div x y = if y = 0 then raise (E 1) else x/y in

try div 5 !(!(!d)) with | E x -> prInt x
