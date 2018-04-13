
(* f e *)
let mkApp f e = (0, App(f, e));;

(* patt -> e *)
let mkFun patt e = (0, Fun(patt, e));;

(* let (a,b) = c in d *)
let mkLetPair (a, b) c d = (0, Let(( (0, Cart([a;b])), c), d);;

(* string -> Identifier string *)
let mkIdentifier varname = (0, Identifier varname);;

let 
