open Expr;;


(*Les fonctions et applications*)

(* f e *)
let mkApp f e = (0, App(f, e) );;
let mkAppxy f x y = mkApp (mkApp f x) y;;
  let mkAppxyz f x y z = mkApp (mkAppxy f x y) z;;

(* patt -> e *)
let mkFun patt e = (0, Fun(patt, e));;
(*pour les continuations, il faut tout le temps définir des fonctions à deux arguments : *)
let mkFunxy x y im = (0, Fun(x, (0, Fun(y, im))));;


(*Les let*)

(*let a = b in c*)
let mkLet a b c  = (0, Let((a, b), c));;
(* let (a,b) = c in d, fait car beaucoup utilisé *)
let mkLetPair (a, b) c d = (0, Let(( (0, Cart([a;b])), c), d));;
let mkLetRec f e1 e2 = (0, LetRec((f,e1),e2));;


(* string -> Identifier string *)
let mkIdentifier varname = (0, Identifier (varname, (0, Typed(0, TypeId "_"))));;

let mkPair (a,b) = (0, Cart([a;b]));;

let mkAdd e1 e2 = (0, Add(e1, e2));;
let mkMul e1 e2 = (0, Mul(e1, e2));;
let mkDiv e1 e2 = (0, Div(e1, e2));;
let mkSou e1 e2 = (0, Sou(e1, e2));;
let mkConst i = (0, Const i);;

let mkCart l = (0, Cart(l));;
let mkConstr name l = (0, Constr(name, l));;
let mkMatch e l = (0, Match(e, l));;
let mkPattCase e1 e2 = (0, PattCase(e1, e2));;

let mkPrintInt e = (0, PrintInt e);;

let mkCond cond e1 e2 = (0, Cond(cond, e1, e2));;

let mkBool comp e1 e2 = match comp with
  | Cond((_,Testeq(_,_) ), _, _) -> (0, Testeq(e1,e2))
  | Cond((_,Testneq(_,_)), _, _) -> (0, Testneq(e1,e2))
  | Cond((_,Testlt(_,_) ), _, _) -> (0, Testlt(e1,e2))
  | Cond((_,Testgt(_,_) ), _, _) -> (0, Testgt(e1,e2))
  | Cond((_,Testlet(_,_)), _, _) -> (0, Testlet(e1,e2))
  | Cond((_,Testget(_,_)), _, _) -> (0, Testget(e1,e2))

  |_ -> failwith "Something gone wrong with constructeur.mkBool"
;;
let mkLVide () = (0, Vide);;
let mkListe x q = (0, Liste(x, q));;

(*Les aspects impératifs*)
let mkUnit () = (0, Uni);;
let mkRef e = (0, Ref e);;
let mkAcc e = (0, Acc e);;
let mkAff e1 e2 = (0, Aff(e1, e2));;

let mkmodify v1 v2 s2 = mkApp (mkApp (mkIdentifier "modify") s2) (mkPair (v1, v2));;
let mkallocate v1 v2 = (mkApp (mkApp (mkIdentifier "allocate") v1) v2);;
let mkread l s1 = (mkApp (mkApp (mkIdentifier "read") l) s1);;



let mkConstr nom lexpr = (0, Constr(nom, lexpr));;
let mkPattCase pattern todo = (0, PattCase(pattern, todo));;


(*Les exceptions*)
let mkTry expression id todo = (0, Try(expression,[mkPattCase id todo]));; (*à compléter pour avoir try a with b -> c *)
let mkExep id = (0, Constr("E", id));;
let mkRaise e s = (0, Raise ((0, Constr("E", [mkPair (e, s)]))));;
