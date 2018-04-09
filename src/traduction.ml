open Affichage;;
open Expr;;
open Env;;
open Errmgr;;



let rec trad_expr ee env =
  debug ee env;
  let (node_id, e) = ee in

  try match e with
      | Add(e1,e2) -> let e1 = trad_expr e1 and e2 = trad_expr e2 in (0, Fun("s0",(0,Let( ((0,Cart([(0,Identifier "v1");(0,Identifier "s1")])), (0,App((0,Identifier "e1"), (0,Identifier "s0")))), ((0,Let( ((0,Cart([(0,Identifier "v2");(0,Identifier "s2")])), (0,App((0,Identifier "e2"), (0,Identifier "s1")))), ((0,Cart([(0,Add((0,Identifier "v1"), (0,Identifier "v2")));(0,Identifier "s2")]))))))))))

      | Mul(e1,e2) -> let e1 = trad_expr e1 and e2 = trad_expr e2 in (0, Fun("s0",(0,Let( ((0,Cart([(0,Identifier "v1");(0,Identifier "s1")])), (0,App((0,Identifier "e1"), (0,Identifier "s0")))), ((0,Let( ((0,Cart([(0,Identifier "v2");(0,Identifier "s2")])), (0,App((0,Identifier "e2"), (0,Identifier "s1")))), ((0,Cart([(0,Mul((0,Identifier "v1"), (0,Identifier "v2")));(0,Identifier "s2")]))))))))))


      | Sou(e1,e2) -> let e1 = trad_expr e1 and e2 = trad_expr e2 in (0, Fun("s0",(0,Let( ((0,Cart([(0,Identifier "v1");(0,Identifier "s1")])), (0,App((0,Identifier "e1"), (0,Identifier "s0")))), ((0,Let( ((0,Cart([(0,Identifier "v2");(0,Identifier "s2")])), (0,App((0,Identifier "e2"), (0,Identifier "s1")))), ((0,Cart([(0,Sou((0,Identifier "v1"), (0,Identifier "v2")));(0,Identifier "s2")]))))))))))

      | Div(e1,e2) -> let e1 = trad_expr e1 and e2 = trad_expr e2 in (0, Fun("s0",(0,Let( ((0,Cart([(0,Identifier "v1");(0,Identifier "s1")])), (0,App((0,Identifier "e1"), (0,Identifier "s0")))), ((0,Let( ((0,Cart([(0,Identifier "v2");(0,Identifier "s2")])), (0,App((0,Identifier "e2"), (0,Identifier "s1")))), ((0,Cart([(0,Div((0,Identifier "v1"), (0,Identifier "v2")));(0,Identifier "s2")]))))))))))

      | Let((x, e1), e2) -> let e1 = trad_expr e1 and e2 = trad_expr e2 in
      (0, Fun("s",(0,Let( ((0,Cart([(0,Identifier "x");(0,Identifier "s1")])), (0,App((0,Identifier "e1"), (0,Identifier "s")))), ((0,App((0,Identifier "e2"), (0,Identifier "s1"))))))))

      | LetRec((x, e1), e2) -> let e1 = trad_expr e1 and e2 = trad_expr e2 in
      (0, Fun("s",(0,LetRec( ((0,Cart([(0,Identifier "x");(0,Identifier "s1")])), (0,App((0,Identifier "e1"), (0,Identifier "s")))), ((0,App((0,Identifier "e2"), (0,Identifier "s1"))))))))
      | App(e1,e2) -> let e1 = trad_expr e1 and e2 = trad_expr e2 in (0,Let( ((0,Identifier "_"), (0,Let( ((0,Identifier "e1"), (0, Fun("x",(0,Identifier "x")))), ((0,Let( ((0,Identifier "e2"), (0, Fun("x",(0,Identifier "x")))), ((0, Fun("s",(0,Let( ((0,Cart([(0,Identifier "f1");(0,Identifier "s1")])), (0,App((0,Identifier "e1"), (0,Identifier "s")))), ((0,Let( ((0,Cart([(0,Identifier "v2");(0,Identifier "s2")])), (0,App((0,Identifier "e2"), (0,Identifier "s1")))), ((0,App((0,App((0,Identifier "f1"), (0,Identifier "v2"))), (0,Identifier "s2")))))))))))))))))), ((0, Const 0))))



(*Là il y a un truc pénible car on n'a pas les booléens en fouine. Il va falloir modifier ce truc pour généraliser à tous les opérateurs*)
      (* | Cond(Testeq(e1,e2),e3,e4) ->let e1 = trad_expr e1 in let e2 = trad_expr e2 in let e3 = trad_expr e3 in let e4 = trad_expr e4 in Fun(s0, Let(Cart(b1,s1), App(e1, s1), Let(Cart(b2,s2), App(e2, s2), Cond(Testeq(b1, b2), App(e3, s2), App(e4, s2))))) *)

  (*aspects impératifs*)
      | Acc(e) -> let e = trad_expr e in
      (0,Let( ((0,Identifier "_"), (0,Let( ((0,Identifier "e"), (0, Fun("x",(0,Identifier "x")))), ((0,Let( ((0,Identifier "read"), (0,Uni)), ((0, Fun("s",(0,Let( ((0,Cart([(0,Identifier "l");(0,Identifier "s1")])), (0,App((0,Identifier "e"), (0,Identifier "s")))), ((0,Let( ((0,Identifier "v"), (0,App((0,App((0,Identifier "read"), (0,Identifier "l"))), (0,Identifier "s1")))), ((0,Cart([(0,Identifier "v");(0,Identifier "s1")]))))))))))))))))), ((0, Const 0))))


      | Aff(nom, e1)-> let nom = trad_expr nom in let e1 = trad_expr e1 in (0,Let( ((0,Identifier "_"), (0, Fun("s0",(0,Let( ((0,Cart([(0,Identifier "v1");(0,Identifier "s1")])), (0,App((0,Identifier "e1"), (0,Identifier "s0")))), ((0,Let( ((0,Cart([(0,Identifier "v2");(0,Identifier "s2")])), (0,App((0,Identifier "e2"), (0,Identifier "s1")))), ((0,Let( ((0,Identifier "s3"), (0,App((0,App((0,Identifier "modify"), (0,Identifier "s2"))), (0,Cart([(0,Identifier "v1");(0,Identifier "v2")]))))), ((0,Cart([(0,Uni);(0,Identifier "s3")])))))))))))))), ((0, Const 0))))

      | Ref(e) ->let e = trad_expr e in (0, Fun("s",(0,Let( ((0,Cart([(0,Identifier "v");(0,Identifier "s1")])), (0,App((0,Identifier "e"), (0,Identifier "s")))), ((0,Let( ((0,Cart([(0,Identifier "l");(0,Identifier "s2")])), (0,App((0,App((0,Identifier "allocate"), (0,Identifier "v"))), (0,Identifier "s1")))), ((0,Cart([(0,Identifier "l");(0,Identifier "s2")]))))))))))



      | x -> (0, Fun("s",(0,Cart((0,x),(0,Identifier "s")))))


  with x -> error_display node_id x; raise Fail
