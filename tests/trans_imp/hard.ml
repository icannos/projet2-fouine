let a = 10 in let b = 8 in let c = ref 5 in let f x = x * !c in c:=8; prInt (f 5) 
