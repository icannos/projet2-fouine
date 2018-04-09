let rec write memoire couple = match memoire with
  | (n,v)::q -> let  (num, newval) = couple in if n = num then ((num, newval))::q else ((n,v))::(write memoire couple)
  |[] -> raise Notfound
in 3
