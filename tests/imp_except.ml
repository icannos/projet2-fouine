let a = ref 0 in try a:=5; raise (E (5)) with E(x) -> x 
