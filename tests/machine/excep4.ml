let div x y = if y = 0 then raise (E 1) else x/y in

try div 5 3  with | E x -> prInt x
