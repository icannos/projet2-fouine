let a = ref 0 in let  f = (a:=5; (fun x -> x)) in prInt !a
