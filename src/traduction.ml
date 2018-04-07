open Affichage;;
open Expr;;
open Env;;
open Errmgr;;



let rec trad_expr ee env =
  debug ee env;
  let (node_id, e) = ee in

  try match e with
     (* | Const k -> Fun(s, Cart(Const k, s))
      | Identifier a -> Fun(s,Cart(Identifier a, s)) (*ce sont des cas identiques ! *) *)

      | Add(e1,e2) -> let e1 = trad_expr e1 and e2 = trad_expr e2 in Fun(Identifier s0, Let(Cart(Identifier v1,Identifier s1), App(e1, Identifier s0), Let(Cart(Identifier v2,Identifier s2), App(e2, Identifier s1), Cart(Mul(Identifier v1, Identifier v2),Identifier s2))))
      | Mul(e1,e2) -> let e1 = trad_expr e1 and e2 = trad_expr e2 in Fun(Identifier s0, Let(Cart(Identifier v1,Identifier s1), App(Identifier e1, Identifier s0), Let(Cart(Identifier v2,Identifier s2), App(Identifier e2, Identifier s1), Cart(Mul(Identifier v1, Identifier v2),Identifier s2))))

      | Sou(e1,e2) ->  let e1 = trad_expr e1 and e2 = trad_expr e2 in Fun(Identifier s0, Let(Cart(Identifier v1,Identifier s1), App(Identifier e1, Identifier s0), Let(Cart(Identifier v2,Identifier s2), App(Identifier e2, Identifier s1), Cart(Sou(Identifier v1, Identifier v2),Identifier s2))))
      | Div(e1,e2) ->  let e1 = trad_expr e1 and e2 = trad_expr e2 in Fun(Identifier s0, Let(Cart(Identifier v1,Identifier s1), App(Identifier e1, Identifier s0), Let(Cart(Identifier v2,Identifier s2), App(Identifier e2, Identifier s1), Cart(Div(Identifier v1, Identifier v2),Identifier s2))))

      | Let((x, e1), e2) -> let e1 = trad_expr e1 and e2 = trad_expr e2 in Fun(Identifier s, Let(Cart(Identifier x,Identifier s1), App(Identifier e1, Identifier s), App(Identifier e2, Identifier s1)))
      | LetRec((x, e1), e2) -> let e1 = trad_expr e1 and e2 = trad_expr e2 in Fun(Identifier s, LetRec(Cart(Identifier x,Identifier s1), App(Identifier e1, Identifier s), App(Identifier e2, Identifier s1)))
      | App(e1,e2) -> let e1 = trad_expr e1 and e2 = trad_expr e2 in Fun(s, Let(Cart(f1,s1), App(e1, s), Let(Cart(v2,s2), App(e2, s1), App(App(f1, v2), s2))))
(*Là il y a un truc pénible car on n'a pas les booléens en fouine. Il va falloir modifier ce truc pour généraliser à tous les opérateurs*)                                                 
      (* | Cond(Testeq(e1,e2),e3,e4) ->let e1 = trad_expr e1 in let e2 = trad_expr e2 in let e3 = trad_expr e3 in let e4 = trad_expr e4 in Fun(s0, Let(Cart(b1,s1), App(e1, s1), Let(Cart(b2,s2), App(e2, s2), Cond(Testeq(b1, b2), App(e3, s2), App(e4, s2))))) *)

  (*aspects impératifs*)
      | Acc(e) -> let e = trad_expr e in Fun(s, Let(Cart(l,s1), App(e, s), Let(v, App(App(read, l), s1), Cart(v, s1))))
      | Aff(nom, e1)-> let nom = trad_expr nom in let e1 = trad_expr e1 in Fun(s0, Let(Cart(v1,s1), App(e1, s0), Let(Cart(v2,s2), App(e2, s1), Let(s3, App(App(write, s2), Cart(v1,v2)), Cart(Uni,s3)))))
      | Ref(e) ->Fun(s, Let(Cart(v,s1), App(e, s), Let(Cart(l,s2), App(App(alloc, v), s1), Cart(l,s2))))
      | x -> Fun(s, Cart(x,s))
  with x -> error_display node_id x; raise Fail
