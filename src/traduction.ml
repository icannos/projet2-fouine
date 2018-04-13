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
 (0,Identifier "s" ^ (string_of_int nbs));;
  
let newv () =
  nbv := !nbv + 1;
 (0,Identifier "v" ^ (string_of_int nbv));;
  
(*des méta-constructeurs qui évitent un code de traduction illisiblesont dans constructeurs*)
  

  
let rec trad_expr ee =
  let (node_id, e) = ee in

  try match e with
      | Add(e1,e2) -> let  s0 = news () in let s1 = news() in let s2 = news() in  let v1 = newv () in let v2 = newv () in let te1 =  trad_expr e1 in let te2 = trad_expr e2 in mkFun s0
    ( mkLetPair (v1,s1) mkApp(te1, s0)
     (mkLetPair(v2,s2) mkApp(te2, s1) mkPair(mkAdd(v1,v2),s2)))
       | Mul(e1,e2) -> let  s0 = news () in let s1 = news() in let s2 = news() in  let v1 = newv () in let v2 = newv () in let te1 =  trad_expr e1 in let te2 = trad_expr e2 in mkFun s0
    ( mkLetPair (v1,s1) mkApp(te1, s0)
     (mkLetPair(v2,s2) mkApp(te2, s1) mkPair(mkMul(v1,v2),s2)))             | Sou(e1,e2) -> let  s0 = news () in let s1 = news() in let s2 = news() in  let v1 = newv () in let v2 = newv () in let te1 =  trad_expr e1 in let te2 = trad_expr e2 in mkFun s0
    ( mkLetPair (v1,s1) mkApp(te1, s0)
     (mkLetPair(v2,s2) mkApp(te2, s1) mkPair(mkSou(v1,v2),s2)))             | Div(e1,e2) -> let  s0 = news () in let s1 = news() in let s2 = news() in  let v1 = newv () in let v2 = newv () in let te1 =  trad_expr e1 in let te2 = trad_expr e2 in mkFun s0
    ( mkLetPair (v1,s1) mkApp(te1, s0)
    (mkLetPair(v2,s2) mkApp(te2, s1) mkPair(mkDiv(v1,v2),s2)))
       | x -> let s = news () in mkFun s mkPair(x, s))                                                                                                                                                                                                                   
  with x -> error_display node_id x; raise Fail
