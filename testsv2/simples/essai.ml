let alloc valeur memoire = match memoire with
  | [] -> (0,[(0,valeur)])
  | (n,v)::q -> (n+1, (n+1,valeur)::(n,v)::q)

let rec read num memoire = match memoire with
  | (n,v)::q -> if n = num then v else read num q
  | [] -> raise Notfound;;

  
let rec write memoire couple = match memoire with
  | (n,v)::q -> let (num, newval)=couple in if n = num then (num, newval)::q else (n,v)::(write memoire couple)
  |[] -> raise Notfound;;