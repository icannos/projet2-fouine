let f x = raise (E x) in
let k = try f 5 with E x -> x + 1 in prInt k
