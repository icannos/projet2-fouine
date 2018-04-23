open Affichage;;
open Expr;;
open Env;;
open Errmgr;;
open Constructeur;;

  (*de même que pour traduction, on va avoir besoin de plein de v, de k et de kE*)

let  nbk =ref 0;;
let  nbkE =ref 0;;
let  nbva = ref 0;;

let  nbs =ref 0;;

let news () =
  nbs := !nbs + 1;
 (0,Identifier ("s" ^ (string_of_int !nbs)));;


let newk () =
  nbk := !nbk + 1;
 (0,Identifier ("k" ^ (string_of_int !nbk)));;

let newkE () =
  nbkE := !nbkE + 1;
 (0,Identifier ("kE" ^ (string_of_int !nbkE)));;

let newva () =
  nbva := !nbva + 1;
 (0,Identifier ("va" ^ (string_of_int !nbva)));;


  let rec cont_expr ee =
  let (node_id, e) = ee in

  try match e with
      (*cas de brique élémentaires, juste les donner à k: fun k kE -> k 52*)
      | Const x -> let k = newk() in let kE = newkE() in
     mkFunxy k  kE (mkApp k (mkConst x))
      | Identifier x ->  let k = newk() in let kE = newkE() in
     mkFunxy k  kE (mkApp k (mkIdentifier x))
      (*autre cas élémentaire : on rencontre une exception (en fait pas si élémentaire vu que ça peut être imbriqué)*)
      | Raise (e) -> let k = newk() in let kE = newkE() in let ce = cont_expr e in
     mkFunxy k kE (mkAppxy ce kE kE)
      (*ensuite les cas où on croit en la récursivité*)
      | Add(e1,e2) -> let k = newk() in let kE = newkE() in let v1 = newva() in let v2 = newva() in
 let ce1 = cont_expr e1 in let ce2 = cont_expr e2 in
     mkFunxy k kE (mkAppxy ce1 (mkFun v1 (mkAppxy ce2 (mkFun v2 (mkApp k ( mkAdd v1 v2 ) ) ) kE )) kE )
      | Mul(e1,e2) -> let k = newk() in let kE = newkE() in let v1 = newva() in let v2 = newva() in
 let ce1 = cont_expr e1 in let ce2 = cont_expr e2 in
  mkFunxy k kE (mkAppxy ce1 (mkFun v1 (mkAppxy ce2 (mkFun v2 (mkApp k ( mkMul v1 v2 ) ) ) kE )) kE )
      | Sou(e1,e2) -> let k = newk() in let kE = newkE() in let v1 = newva() in let v2 = newva() in
 let ce1 = cont_expr e1 in let ce2 = cont_expr e2 in
     mkFunxy k kE (mkAppxy ce1 (mkFun v1 (mkAppxy ce2 (mkFun v2 (mkApp k ( mkSou v1 v2 ) ) ) kE )) kE )
      | Div(e1,e2) -> let k = newk() in let kE = newkE() in let v1 = newva() in let v2 = newva() in
 let ce1 = cont_expr e1 in let ce2 = cont_expr e2 in
                           mkFunxy k kE (mkAppxy ce1 (mkFun v1 (mkAppxy ce2 (mkFun v2 (mkApp k ( mkDiv v1 v2 ) ) ) kE )) kE )

      | PrintInt e -> let k = newk() in let kE = newkE() in
      let ce = cont_expr e in let x = newva() in
     mkFunxy k kE (mkAppxy ce (mkFun x ( (mkPrintInt x)) ) kE)


      | Fun(patt, e) -> let k = newk() in let kE =newkE() in let ce = cont_expr e in
      let x1 = newva () in
  mkFunxy k kE (mkApp k (mkFun patt  (mkAppxy ce (mkFun x1 x1) kE)))

      | App(e1, e2) -> let f = newva() in let v = newva() in let k = newk() in let kE = newkE() in
      let ce1 = cont_expr e1 in let ce2 = cont_expr e2 in
  mkFunxy k kE (mkAppxy ce2 (mkAppxy ce1 k kE) kE)


      | Try(e1,e2)-> let k = newk() in let x = newk () in let kE= newkE() in let ce1 = cont_expr e1 in let (_,PattCase((_, y),x) )=  (List.hd e2)in let ce2 = cont_expr x in
  mkFunxy k kE (mkAppxy ce1 k (mkFun (0,y) (mkAppxy ce2 k kE)))

      |Let((patt, e1), e2) -> let te1 = cont_expr e1 in let te2 = cont_expr e2 in  let k = newk () in let ke = newk () in
      let s0 = news() in
      mkFunxy k ke (mkAppxy te1 (mkFun s0 (mkLet patt s0 (mkAppxy te2 k ke))) ke)

      |LetRec((nom, e1), e2) -> let te1 = cont_expr e1 in let te2 = cont_expr e2 in  let k = newk () in let ke = newk () in
      let s0 = news() in
      mkFunxy k ke (mkAppxy te1 (mkFun s0 (mkLetRec nom s0 (mkAppxy te2 k ke))) ke)

       |Cond((_, Testeq(e1,e2)), e3, e4)
       |Cond((_,Testneq(e1,e2)), e3, e4)
       |Cond((_,Testlt(e1,e2)), e3, e4)
       |Cond((_,Testgt(e1,e2)), e3, e4)
       |Cond((_,Testlet(e1,e2)), e3, e4)
       |Cond((_,Testget(e1,e2)), e3, e4)
       -> let tc1 = cont_expr e1 in let tc2 = cont_expr e2 in
        let te1 = cont_expr e3 in let te2 = cont_expr e4 in  let k = newk () in let ke = newk () in
      let s0 = news() in let s1 = news () in
      mkFunxy k ke (mkAppxy tc2 (mkFun s0 (mkAppxy tc1 (mkFun s1  (mkCond (mkBool e s0 s1) (mkAppxy te1 k ke) (mkAppxy te2 k ke))) ke)) ke)



   with x -> error_display node_id x; raise Fail

;;


  let exec_excep ast = let x1 = newva () and x2 = newva () in mkAppxy (cont_expr ast) (mkFun x1 x1)  (mkFun x2 x2) ;;
