let rec write memoire (num, newval) = match memoire with
  | (n,v)::q -> if n = num then (num, newval)::q else (n,v)::(write memoire (num, newval))
  |[] -> raise Notfound;;
