let incr x =
	x := !x +1; !x
in
let x = ref 0 in
let (a, b) = (incr x, incr x) in
	(prInt a; prInt b)