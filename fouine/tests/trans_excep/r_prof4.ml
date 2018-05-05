let y = ref 0 in

let f x =

  let _ = y := !y + 1 in x

in

let a = try f (raise (E 5)) with E u -> !y in

let g =

  let _ = y := !y + 1 in

  fun x -> x

in

let b = try g (raise (E 5)) with E u -> !y in

prInt (a + b)