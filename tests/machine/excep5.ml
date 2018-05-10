let a = try raise (E 1) with E x -> prInt x in prInt a
