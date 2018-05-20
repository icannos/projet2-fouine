let f x = x*x in let g = ref f in prInt (!g 12); g
