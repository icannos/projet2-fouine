open Affichage;;
open Expr;;
open Env;;
open Display;;
open Safe;;
open Memory;;



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
  (* Ici on traite les cas impératifs  *)
  (* Si on tente un accès mémoire, on récupère la référence associée et donc l'adresse en mémoire, puis on lit là où il faut *)
  | Acc(nom) -> let Reference(addr) = Environnement.find nom env in read_address addr
  (* Pour l'affectation on récupère de même l'adresse associée au nom dans l'environnement, puis on ajoute dans la mémoire l'évaluation de l'expression, on retourne ici un nouveau type Unit qui correspond au unit de caml *)
  | Aff(nom, e) -> let Reference(addr) = Environnement.find nom env in add_memory addr (eval e env); Unit
  (* Créer une référence revient à trouver une nouvelle adresse, ajouter à cette adresse l'evaluation de l'expression puis renvoyer un truc  Reference(addr)  *)
  | Ref(e) -> let addr = new_address () in add_memory addr (eval e env); Reference(addr)
                                                                 
  | Identifier nom -> begin  try (Environnement.find nom env) with Not_found -> ps "hey: " ;ps nom; (Int 0) end

  | PrintInt e -> let Int x = (eval e env) in Int (prInt x)
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
                  |Reference(_) -> failwith "cannot apply a ref" (* On râle si on essaie d'appliquer une référence *)

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
