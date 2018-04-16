open Expr;;
(* f e *)
let mkApp f e = (0, App(f, e) );;

(* patt -> e *)
let mkFun patt e = (0, Fun(patt, e));;

(*let a = b in c*)
let mkLet a b c  = (0, Let((a, b), c));;
(* let (a,b) = c in d, fait car beaucoup utilisé *)
let mkLetPair (a, b) c d = (0, Let(( (0, Cart([a;b])), c), d));;

(* string -> Identifier string *)
let mkIdentifier varname = (0, Identifier varname);;

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


let mkLVide () = (0, Vide);;
let mkListe x q = (0, Liste(x, q));;
