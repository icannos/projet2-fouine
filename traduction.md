##Pour les continuations


[x] = fun k kE -> k x
[Raise e] = fun k kE -> [e] kE kE
[Try e1 with E x -> e2] = fun k kE -> [e1] k (fun x -> [e2] k kE)

[e1 + e2] = fun k kE -> [e1] (fun v1 -> ([e2] (fun v2 -> k (v1 + v2)) kE)) kE
[Print e] = fun k kE -> [e] (fun x -> print x) kE

[fun patt -> e] = fun k kE -> k (fun x -> e) (tellement évident que ça ne marche pas...)
[e1 e2] = fun k kE -> [e2] (fun f -> ([e1] (fun v -> k (f x)) kE)) kE (moi je crois en celle là mais tu as modifié pour mettre l'autre)
ou
[e1 e2] = fun k kE -> [e2] ([e1] k kE) kE

[let patt = e1 in e2 ] = fun k kE -> [e1] (x -> let patt  = x in ([e2] k kE)) kE
[if e1 op e2 then e3 else e4 ] = fun k kE -> [e2] (fun x -> [e1] (fun y -> if x op y then [e3] k kE else [e4] k kE) kE) kE


(ne marche pas)
[Ref e] = fun k kE -> [e] (fun x -> Ref x) kE
[!e] = fun k kE -> [e] (fun x -> !x)kE 
