open Affichage;;
open Expr;;
open Env;;
open Display;;
open Safe;;
open Memory;;
open Errmgr;;

let buildEnv nom env expr =
  let nenv = ref (Environnement.empty) in
  let addVar key  =
    nenv := Environnement.add key (Environnement.find key env) (!nenv)
  in


  let freeV = freevars (VarsSet.singleton nom) (VarsSet.empty) expr in

  VarsSet.iter addVar freeV; !nenv
;;

let unification expr v env = (*fonction de type expr-> value-> env -> env, ajoute dans l'environnement l'unification de expr avec v si c'est possible, si l'unification est impossible on lève l'exception UnificationFails, à remplir un jour*)
  let envir = ref env in
  let rec unif (expr,v) = match expr, v with
    |(_,Identifier key), x when key <> "_"-> envir := (Environnement.add key x (!envir))
    |(_,Identifier key), x when key = "_"-> ()
    |(_, Const x), Int y when x = y -> ()

    |(_,Constr(c1, exprlist)), TSum(c2, vlist)
         when (c1 = c2) && ((List.length exprlist) = (List.length vlist))
                                                  -> List.iter unif (List.combine exprlist vlist)
    |(_,Cart(exprlist)), Cartesian(vlist) when (List.length exprlist)=(List.length vlist)
     -> List.iter unif (List.combine exprlist vlist)
    |(_,Vide), LVide -> ()
    |(_,Liste(t,q)), Listing(a,b) -> unif (t,a); unif (q,b)
    |_ -> raise (UnificationFails (string_of_expr expr, string_of_value v))
  in
  unif (expr,v); !envir
;;

(*fonction value -> liste de Patcase -> env -> (expr, env) renvoie l'expression qui correspond au premier matching qui convient, et l'environnement associé. En cas d'aucun matching possible, on lève à nouveau l'expression UnificationFails *)
let rec trymatch value caselist env = match caselist with
  |[] -> raise (UnificationFails("", ""))
  |[(_, PattCase(patt, x))] -> (x, unification patt value env)
  |(_, PattCase(patt, x))::q -> begin try (x,unification patt value env) with UnificationFails(_, _) -> trymatch value q env end
  | _ -> raise (UnificationFails("",""))
;;


(* sémantique opérationnelle à grands pas *)

(* eval -> expr -> env -> value*)
let rec eval ee env  =
  debug ee env;
  let (node_id, e) = ee in

  try
    match e with
    | Const k -> Int k
    (* Ici on traite les cas impératifs  *)
    (* Si on tente un accès mémoire, on récupère la référence associée et donc l'adresse en mémoire, puis on lit là où il faut *)
    | Acc(e) ->
       begin  match eval e env with
            |Exn x -> Exn x
            |Reference(addr)->(try read_address addr with Not_found -> raise (UnknownReference (string_of_expr e)))
            | _ -> failwith "Not a reference"
       end
    (* Pour l'affectation on récupère de même l'adresse associée au nom dans l'environnement, puis on ajoute dans la mémoire l'évaluation de l'expression, on retourne ici un nouveau type Unit qui correspond au unit de caml *)
    | Aff(nom, e) ->
       begin match Environnement.find nom env with
            |Reference(addr)->(try add_memory addr (eval e env); Unit
                                                     with Not_found ->  raise (UnknownReference (string_of_expr e)))
            |_ -> failwith "Not a reference"
       end
    (* Créer une référence revient à trouver une nouvelle adresse, ajouter à cette adresse l'evaluation de l'expression puis renvoyer un truc  Reference(addr)  *)
    | Ref(e) -> let addr = new_address () in begin match eval e env with |Exn x -> Exn x |v -> add_memory addr v; Reference(addr) end

    | Identifier nom ->
       begin
        try (Environnement.find nom env)
        with Not_found -> raise (UnknownIdentifier nom)
       end

    (* |Cart(exprlist) -> Cartesian (List.map (fun x -> eval x env) exprlist) *)
    |Cart([]) -> Cartesian([])
    |Cart(exprlist) -> begin match eval (List.hd exprlist) env with |Exn x -> Exn x |v -> begin match eval (node_id, Cart(List.tl exprlist)) env with |Exn x -> Exn x |Cartesian(t) -> Cartesian(v::t) | _ -> failwith "Not a cart" end end

    (* |Constr(cons, exprlist) -> TSum(cons, (List.map (fun x -> eval x env) exprlist)) *)
    |Constr(c, []) -> TSum(c, [])
    |Constr(cons, exprlist) -> begin match eval (List.hd exprlist) env with |Exn x -> Exn x |v -> begin match eval (node_id, Constr(cons, List.tl exprlist)) env with |Exn x -> Exn x |TSum(c, t) -> TSum(c, v::t) | _ -> failwith "Not a constr" end end
    |Vide -> LVide
    |Liste(t,q)-> (match eval t env with
                  |Exn x -> Exn x
                  |x -> (match eval q env with Exn x -> Exn x |y -> Listing(x, y))
                  )

    |Match(expr, exprlist) -> (try let e, envir = trymatch (eval expr env) exprlist env in
                                   eval e envir with UnificationFails (_,_) -> raise PatternMatchingFails)

    |Try(expr, exprlist) -> begin match (eval expr env) with
                            |Exn x  -> (try let e, envir = trymatch x exprlist env in
                              eval e envir with UnificationFails (_,_) -> Exn x)
                            |x -> x
                            end

    |Raise(expr) -> begin match eval expr env with |Exn x -> Exn x |v -> Exn v end


    | PrintInt e -> begin match eval e env with
                    | Int x -> Int (prInt x)
                    | Exn x -> Exn x
                    | _ -> raise (BadArgument(string_of_value (eval e env), "prInt"))
                    end
    | Add(e1,e2) -> safe_add (eval e1 env) (eval e2 env)
    | Mul(e1,e2) -> safe_mult (eval e1 env) (eval e2 env)
    | Sou(e1,e2) -> safe_sou (eval e1 env) (eval e2 env)
    | Div(e1,e2) -> safe_div (eval e1 env) (eval e2 env)
    | Let((pattern, e1), e2) -> evallet pattern e1 e2 env
    | LetRec((nom, e1), e2) -> evalletrec nom e1 e2 env
    | Cond(booleen,e1,e2) -> begin match evalb booleen env with |Exn x -> Exn x |Bool b -> if b  then (eval e1 env) else (eval e2 env) | _ -> failwith "Not a boolean" end
    | Uni -> Unit

    |Fun(argument, expr) -> Fonction(argument, expr, buildEnv argument env expr) (*de type name * expr * env*)
    |App(e1, e2) -> evalapp e1 e2 env

    |PattCase(_,_)-> failwith "Pattcase without match"
  with x -> error_display node_id x; raise Fail
(* evalb de type bexpr -> env -> bool*)

and evalb ee env =
  let node_id, e = ee in
  match e with
  | Testeq(e1,e2) ->  safe_op (eval e1 env) (=) (eval e2 env)
  | Testneq(e1,e2) -> safe_op (eval e1 env) (<>) (eval e2 env)
  | Testlt(e1,e2) ->  safe_op (eval e1 env) (<) (eval e2 env)
  | Testgt(e1,e2) ->  safe_op (eval e1 env) (>) (eval e2 env)
  | Testlet(e1,e2) -> safe_op (eval e1 env) (<=) (eval e2 env)
  | Testget(e1,e2) -> safe_op (eval e1 env) (>=) (eval e2 env)

(*on unifie le pattern avec e1*)
  and evallet patt e1 e2 env = match eval e1 env with
  |Exn x -> Exn x
  |v -> let envir = unification patt v  env in eval e2 envir

  and evalletrec  nom ee1 ee2 env = match nom with
    |"_" -> let _ = eval ee1 env in eval ee2 env
    |_ -> begin
       let _, e1 = ee1 in
       match e1 with
       (* J'ajoute un Int 0 à la place de f histoire q'il connaisse f dans l'environnement lorsqu'il construit la cloture, mais de toutes façons f est remplacée dans l'environnement lors de l'application *)
        |Fun(arg, fexpr) ->  let envir = Environnement.add nom (Rec(nom, arg, fexpr, (buildEnv arg (Environnement.add nom (Int 0) env) fexpr))) env in  eval ee2 envir
        |_ -> raise (NotFunction (string_of_expr ee1))
      end

  and evalapp e1 e2  env =  match  eval e1 env with (* On ajoute à chaque application dans l'environnement d'éxécution de la fonction récursive, elle même pour qu'elle puisse se trouver elle même lors de l'exécution*)
                    |Exn x -> Exn x
                    |Fonction("_", expr, fenv) ->  eval expr fenv
                    |Fonction(argument, expr, fenv) ->  eval expr (Environnement.add argument (eval e2 env) fenv) (*on remplace le xpar la valeur d'appel*)
                    |Rec(nom, arg, fexpr, fenv) -> let recenv = Environnement.add nom (Rec(nom, arg, fexpr, fenv)) fenv in  eval fexpr (Environnement.add arg (eval e2 env) recenv)
                    |Int k -> raise (CannotApply "integer")
                    |Reference(_) -> raise (CannotApply "a reference") (* On râle si on essaie d'appliquer une référence *)
                    |Unit -> raise (CannotApply "unit")
                    |LVide -> raise (CannotApply "lvide")
                    |TSum(_,_) -> raise (CannotApply "tsum")
                    |Cartesian _ -> raise (CannotApply "cartesian")
                    |Listing(_,_) -> raise (CannotApply "a list")
                    |Bool _ -> raise (CannotApply "a boolean")

;;
