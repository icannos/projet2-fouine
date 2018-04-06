open Affichage;;
open Expr;;
open Env;;
open Display;;
open Safe;;
open Memory;;
open Errmgr;;

(*les trois fonctions de gestion mémoire à inclure au début de la traduction*)

(*alloc de type unit -> loc*)
(*read de type loc -> value*)
(*modify de type loc -> value -> unit*)


let rec trad_expr ee =
  debug ee env;
  let (node_id, e) = ee in

  try match e with
      | Const k -> Fun(s, Cart(Const k, s))
      | Identifier a -> Fun(s,Cart(Identifier a, s))
(*fun s0 -> let (v1,s1) = [e1] s0 in let (v2,s2)=[e2] s1 in (v1+v2, s2)*)
      | Add(e1,e2) -> let e1 = trad_expr e1 and e2 = trad_expr e2 in Fun(s0, Let(Cart(v1,s1), App(e1, s0), Let(Cart(v2,s2), App(e2, s1), Cart(Add(v1, v2)s2))))
      | Mul(e1,e2) -> let e1 = trad_expr e1 and e2 = trad_expr e2 in Fun(s0, Let(Cart(v1,s1), App(e1, s0), Let(Cart(v2,s2), App(e2, s1), Cart(Mul(v1, v2)s2))))
      | Sou(e1,e2) ->  let e1 = trad_expr e1 and e2 = trad_expr e2 in Fun(s0, Let(Cart(v1,s1), App(e1, s0), Let(Cart(v2,s2), App(e2, s1), Cart(Sou(v1, v2)s2))))
      | Div(e1,e2) ->  let e1 = trad_expr e1 and e2 = trad_expr e2 in Fun(s0, Let(Cart(v1,s1), App(e1, s0), Let(Cart(v2,s2), App(e2, s1), Cart(Div(v1, v2)s2))))
      | Let((pattern, e1), e2) -> let e1 = trad_expr e1 and e2 = trad_expr e2 in Fun(s, Let(Cart(x,s1), App(e1, s), App(e2, s1)))
      | LetRec((nom, e1), e2) -> let e1 = trad_expr e1 and e2 = trad_expr e2 in Fun(s, LetRec(Cart(x,s1), App(e1, s), App(e2, s1)))
      | App(e1,e2) -> let e1 = trad_expr e1 and e2 = trad_expr e2 inFun(s, Let(Cart(f1s1), App(e1, s), Let(Cart(v2,s2), App(e2, s1), App(App(f1, v2), s2))))
(*Là il y a un truc pénible car on n'a pas les booléens en fouine. Il va falloir modifier ce truc pour généraliser à tous les opérateurs*)                                                 
     | Cond(Testeq(e1,e2),e3,e4) ->let e1 = trad_expr e1 in e2 = trad_expr e2 in let e3 = trad_expr e3 in let e4 = trad_expr e4 in Fun(s0, Let(Cart(b1,s1), App(e1, s1), Let(Cart(b2,s2), App(e2, s2), Cond(Testeq(b1, b2), App(e3, s2), App(e4, s2)))))

                                                                                    (*aspects impératifs*)
      | Acc(e) -> let e = trad_expr e in Fun(s, Let(Cart(l,s1), App(e, s), Let(v, App(read l, s1), Cart(v, s1))))
      | Aff(nom, e1)-> let nom = trad_expr nom in let e1 = trad_expre1 in Fun(s0, Let(Cart(v1,s1), App(e1, s0), Let(Cart(v2,s2), App(e2, s1), Let(s3, modify s2 Cart(v1,v2), Cart(Uni,s3)))))
      | Ref(e) ->Fun(s, Let(Cart(v,s1), App(e, s), Let(Cart(l,s2), App(alloc v, s1), Cart(ls2))))

  with x -> error_display node_id x; raise Fail
