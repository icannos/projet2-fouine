open Expr;;
(* f e *)
let mkApp f e = (0, App(f, e) );;

(* patt -> e *)
let mkFun patt e = (0, Fun(patt, e));;

(* let (a,b) = c in d *)
let mkLetPair (a, b) c d = (0, Let(( (0, Cart([a;b])), c), d));;

(* string -> Identifier string *)
let mkIdentifier varname = (0, Identifier varname);;

let mkPair (a,b) = (0, Cart([a;b]));;

let mkAdd e1 e2 = (0, Add(e1, e2));;
let mkMul e1 e2 = (0, Mul(e1, e2));;
let mkDiv e1 e2 = (0, Div(e1, e2));;
let mkSou e1 e2 = (0, Sou(e1, e2));;
