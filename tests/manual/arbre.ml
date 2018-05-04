let rec print_list l = match l with
|[] -> ()
|x::q -> prInt x; print_list q
;;

let rec concat la lb = match la with
|[] -> lb
|x::q -> x::(concat q lb)
;;

let rec bfs arbre = match arbre with
| Feuille(x) -> [x]
| Noeud(ag, x, ad) -> x::(concat (bfs ag) (bfs ad))
;;

print_list (bfs (Noeud(Noeud(Noeud(Feuille(4),3,Noeud(Feuille(6),8,Feuille(9))),2, Noeud(Feuille(6),8,Feuille(9))),1,
Noeud(Noeud(Feuille(4),3,Noeud(Feuille(6),8,Feuille(9))),2, Noeud(Feuille(6),8,Feuille(9)))) ))
