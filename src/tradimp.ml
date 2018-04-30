open Affichage;;
open Expr;;
open Env;;
open Errmgr;;
open Constructeur;;

(*deux références globales qui permettront d'éviter les conflits dans la traduction*)
let  nbv =ref 0;;
let  nbs =ref 0;;

(*fonction qui renvoie un sn où n est un numéro par encore utilisé*)
let news () =
  nbs := !nbs + 1;
 (0,Identifier ("s" ^ (string_of_int !nbs)));;

let newv () =
  nbv := !nbv + 1;
 (0,Identifier ("v" ^ (string_of_int !nbv)));;

(*des méta-constructeurs qui évitent un code de traduction illisible sont dans constructeurs*)

let rec trad_expr ee =
  let (node_id, e) = ee in

  try match e with
     (* | Const x -> let s0 = news () in mkFun s0 (mkPair ((mkConst x), s0))
      | Identifier x -> let s0 = news () in mkFun s0 (mkPair ((mkIdentifier x), s0)) (*j'ai synthétiser les deux cas dans le dernier, on peut  discuter ce choix*)*)
      | PrintInt e -> let s0 = news () and s1 = news () and v1 = newv () and te = trad_expr e in
      mkFun s0 (mkLetPair (v1, s1) (mkApp te s0) (mkPair(mkPrintInt v1, s1)))


      | Add(e1,e2) -> let  s0 = news () in let s1 = news() in let s2 = news() in  let v1 = newv () in let v2 = newv () in let te1 =  trad_expr e1 in let te2 = trad_expr e2 in
      mkFun s0 ( mkLetPair (v1,s1) (mkApp te1 s0) (mkLetPair (v2,s2) (mkApp te2 s1) (mkPair ((mkAdd v1 v2),s2))))
      | Mul(e1,e2) -> let  s0 = news () in let s1 = news() in let s2 = news() in  let v1 = newv () in let v2 = newv () in let te1 =  trad_expr e1 in let te2 = trad_expr e2 in
      mkFun s0 ( mkLetPair (v1,s1) (mkApp te1 s0) (mkLetPair (v2,s2) (mkApp te2 s1) (mkPair ((mkMul v1 v2),s2))))
      | Sou(e1,e2) -> let  s0 = news () in let s1 = news() in let s2 = news() in  let v1 = newv () in let v2 = newv () in let te1 =  trad_expr e1 in let te2 = trad_expr e2 in
      mkFun s0 ( mkLetPair (v1,s1) (mkApp te1 s0) (mkLetPair (v2,s2) (mkApp te2 s1) (mkPair ((mkSou v1 v2),s2))))
      | Div(e1,e2) -> let  s0 = news () in let s1 = news() in let s2 = news() in  let v1 = newv () in let v2 = newv () in let te1 =  trad_expr e1 in let te2 = trad_expr e2 in
 mkFun s0 ( mkLetPair (v1,s1) (mkApp te1 s0) (mkLetPair (v2,s2) (mkApp te2 s1) (mkPair ((mkDiv v1 v2),s2))))

      | Let((patt, e1),e2) -> let s0 = news() in let s1 = news() in let te1 = trad_expr e1 in let te2 = trad_expr e2 in
 mkFun s0 (mkLetPair (patt, s1) (mkApp te1 s0) (mkApp te2 s1) )
      |Fun(patt,e) -> let s0 = news()  in let te = trad_expr e in
  mkFun s0 (mkPair (mkFun patt te, s0))
      |Ref(e) -> let s0 = news() in let s1 = news() in let s2 = news() in let v0 = newv() in let l = newv () in let te = trad_expr e in
    mkFun s0 (mkLetPair (v0,s1) (mkApp te s0) ((mkLetPair (l,s2) ((mkallocate v0 s1))) (mkPair (l,s2)) ))
      |Acc(e) -> let s0 = news() in let s1 = news() in let v = newv() in let l = newv () in let te = trad_expr e in
                                                                         mkFun s0 (mkLetPair (l,s1) (mkApp te s0) (mkLet v (mkread l s1) (mkPair (v,s1)) ))
      |Aff(nom, e)-> let s0 = news() in let s1= news() in let s2 = news() in let s3 =news() in let v1 = newv() in let v2 = newv() in let tnom = trad_expr nom in let te = trad_expr e in
   mkFun s0 (mkLetPair (v1,s1) (mkApp tnom s0) (mkLetPair (v2,s2) (mkApp te s1) (mkLet s3 (mkmodify v1 v2 s2) (mkPair (mkUnit (), s3)))))
      |Cond((_, Testeq(e1,e2)), e3, e4)
       |Cond((_,Testneq(e1,e2)), e3, e4)
       |Cond((_,Testlt(e1,e2)), e3, e4)
       |Cond((_,Testgt(e1,e2)), e3, e4)
       |Cond((_,Testlet(e1,e2)), e3, e4)
       |Cond((_,Testget(e1,e2)), e3, e4) -> let s0 = news() in let s1 = news() in let s2 = news() in let b1 = newv() in let b2 = newv() in
       let te1 = trad_expr e1 in let te2 = trad_expr e2 in let te3 = trad_expr e3 in let te4 = trad_expr e4 in
  mkFun s0 (mkLetPair (b1,s1) (mkApp te1 s0)(mkLetPair (b2,s2) (mkApp te2 s1) (mkCond (mkBool e b1 b2) (mkApp te3 s2) (mkApp te4 s2))))
    |App(e1, e2) -> let s0 = news() in let s1 = news() in let s2 = news() in let f = newv() in let v = newv() in let te1 = trad_expr e1 in let te2 = trad_expr e2 in
                                                                                                                                           mkFun s0 (mkLetPair (f,s1) (mkApp te1 s0) (mkLetPair (v,s2) (mkApp te2 s1) (mkApp (mkApp f v) s2) )  )

    |LetRec((nom,e1),e2) -> let v1 = newv() in let s0 = news() in let s1 = news() in let te1 = trad_expr e1 in let te2 = trad_expr e2 in
                mkFun s0 (mkLetPair (v1,s1) (mkLet (mkIdentifier nom) (mkConst 0) (mkApp te1 s0)) (mkLetRec nom v1 (mkApp te2 s1) ))
    |Try(e1,e2) -> let te1 = trad_expr e1 in let  (_,PattCase((_, Constr("E", [(_, y)])),x) ) =  (List.hd e2) in let te2 = trad_expr x in let s0 = news() in let s1 = news() in
                    mkFun s0 (mkTry (mkApp te1 s0) (mkExep [s1; (0,y)]) (mkApp te2 s1))
    | Raise e -> let s0 = news() in let (_, Constr("E", [x])) = e in let te = trad_expr e in mkFun s0 (mkRaise te s0)
    (*fun s0 -> try e1 s0 with E (x,s1) -> e2 s1*)                      (*fun s0 -> raise (e1,s0)*)

    |x -> let s0 = news () in mkFun s0 (mkPair ((0, x),s0))

  with x -> error_display node_id x; raise Fail

;;

let exec_trad ast = mkApp (trad_expr ast) (0, Vide);;
