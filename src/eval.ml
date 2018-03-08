open Affichage;;
open Expr;;
open Env;;
open Display;;
open Safe;;



let buildEnv nom env expr =
  let nenv = ref (Environnement.empty) in
  let addVar key  =
    nenv := Environnement.add key (Environnement.find key env) (!nenv)
  in

  let freeV = freevars (VarsSet.singleton nom) (VarsSet.empty) expr in

  VarsSet.iter addVar freeV; !nenv
;;
     


(* sémantique opérationnelle à grands pas *)
(*modifions le type de cette fonction, désormais eval -> expr -> env -> value*)

let rec eval e env  =
  debug e env;

  match e with
  | Const k -> Int k
  | Identifier k -> begin  try (Environnement.find k env) with Not_found -> ps "hey: " ;ps k; (Int 0) end

  | PrintInt e -> let Int x = (eval e env) in Int (prInt x)
  (* | Seq(e1,e2) -> eval e1 env; *)
  | Add(e1,e2) -> safe_add (eval e1 env) (eval e2 env)
  | Mul(e1,e2) -> safe_mult (eval e1 env) (eval e2 env)
  | Sou(e1,e2) -> safe_sou (eval e1 env) (eval e2 env)
  | Div(e1,e2) -> safe_div (eval e1 env) (eval e2 env)  
  | Let((nom, e1), e2) -> evallet  nom e1 e2 env
  | LetRec((nom, e1), e2) -> evalletrec nom e1 e2 env
  | Cond(booleen,e1,e2) -> if (evalb booleen env) then (eval e1 env) else (eval e2 env) 
                         

                         
  |Fun(argument, expr) -> Fonction(argument, expr, buildEnv argument env expr) (*de type name * expr * env*)
  |App(e1, e2) -> match  eval e1 env with (* On ajoute à chaque application dans l'environnement d'éxécution de la fonction récursive, elle même pour qu'elle puisse se trouver elle même lors de l'exécution*)
                  |Fonction(argument, expr, fenv) ->  eval expr (Environnement.add argument (eval e2 env) fenv) (*on remplace le xpar la valeur d'appel*)
                  |Rec(nom, arg, fexpr, fenv) -> let recenv = Environnement.add nom (Rec(nom, arg, fexpr, fenv)) fenv in  eval fexpr (Environnement.add arg (eval e2 env) recenv)
                  |Int k -> Int k  (*cas des fonctions constantes *)

(* evalb de type bexpr -> env -> bool*)         
and evalb e env = match e with  (*à réécrire bientot*)
  | Testeq(e1,e2) ->  safe_op (eval e1 env) (=) (eval e2 env)  
  | Testneq(e1,e2) -> safe_op (eval e1 env) (<>) (eval e2 env)
  | Testlt(e1,e2) ->  safe_op (eval e1 env) (<) (eval e2 env)
  | Testgt(e1,e2) ->  safe_op (eval e1 env) (>) (eval e2 env)
  | Testlet(e1,e2) -> safe_op (eval e1 env) (<=) (eval e2 env)
  | Testget(e1,e2) -> safe_op (eval e1 env) (>=) (eval e2 env)
                    
  and evallet nom e1 e2 env = match nom with
    |"_" -> let _ = eval e1 env in eval e2 env
    |_ -> let envir = Environnement.add nom (eval e1 env) env in eval e2 envir

  and evalletrec nom e1 e2 env = match nom with
    |"_" -> let _ = eval e1 env in eval e2 env
    |_ -> begin
       
        match e1 with (* J'ajoute un Int 0 à la place de f histoire q'il connaisse f dans l'environnement lorsqu'il construit la cloture, mais de toutes façons f est remplacée dans l'environnement lors de l'application *)
        |Fun(arg, fexpr) ->  let envir = Environnement.add nom (Rec(nom, arg, fexpr, (buildEnv arg (Environnement.add nom (Int 0) env) fexpr))) env in  eval e2 envir
        |_ -> failwith "That's not a function !"
      end
                                         


;;
