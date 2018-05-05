let div a b = if b = 0 then raise(E(1)) else a/b in

   let a = 5 in let b = 0 in
        try div a b with | E(x) -> prInt x
