let f g x =

  try g x

  with E y -> raise (E (y + 1))

 let h x = raise (E x)

;;

prInt (try f h 1 with E y -> y) 
