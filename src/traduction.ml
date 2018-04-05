open Affichage;;
open Expr;;
open Env;;
open Display;;
open Safe;;
open Memory;;
open Errmgr;;


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
      | App(e1,e2) -> let e1 = trad_expr e1 and e2 = trad_expr e2 inFun(s, Let(Cart(f1s1), App(e1, s), Let(Cart(v2s2), App(e2, s1), App(App(f1, v2), s2))))                                   
      | Cond(booleen,e1,e2) ->

  with x -> error_display node_id x; raise Fail
