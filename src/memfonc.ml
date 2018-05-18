

  (**Réécriture en fonctionnel pur : la mémoire est désormais un couple une fonction et le nombre d'image (ie le premier entier qui n'a pas encore image*)
let allocate valeur memoire =
  let (f, n) = memoire in
  let fbis x = if (x = n) then valeur
               else f x
  in
  (n, (fbis, n+1))
;;


let  read num memoire =
  let (f,n) = memoire in
  if (num < n) then f num
  else -1
;;

let modify memoire couple =
  let (num, newval) = couple in
  let (f,n) = memoire in
  if (num < n) then
    let fbis x = if (x = num) then newval
                 else f x in (fbis, n)
  else
    (f, n)
;;
