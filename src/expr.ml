(* un type pour toutes les expressions qu'on manipule *)

open Env;;
open Arguments;;
open Display;;



type expr =

  (* Arith Constr *)
  | Const of int
  | Add of expr*expr
  | Mul of expr*expr
  | Sou of expr*expr
  | Div of expr*expr

  (* Seq *)
  |Seq of expr*expr

  (* Binding constr  *)
  | Let of name * expr * expr
  | Identifier of name
  | Fun of name * expr
  | App of expr * expr

  (* Built in *)
  |PrintInt of expr


  (* Tests Constructor *)
  |Cond of expr * expr * expr
          
  | Testeq of expr * expr
  | Testneq of expr * expr
  | Testlt of expr * expr
  | Testgt of expr * expr
  | Testlet of expr * expr
  | Testget of expr * expr
;;

                 
(* fonction d'affichage *)
let rec affiche_expr e =
  let aff_aux s a b = 
      begin
	print_string s;
	affiche_expr a;
	print_string ", ";
	affiche_expr b;
	print_string ")"
      end
  in
  match e with
  | Identifier s -> ps s
  | Const k -> print_int k
  | Add(e1,e2) -> aff_aux "Add(" e1 e2
  | Mul(e1,e2) -> aff_aux "Mul(" e1 e2
  | Sou(e1,e2) -> aff_aux "Sou(" e1 e2
  | Div(e1,e2) -> aff_aux "Div(" e1 e2
  | Let(nom,e1,e2) ->
        print_string "Let(";
        print_string nom;
	print_string ", ";
	affiche_expr e1;
	print_string ", ";
	affiche_expr e2;
	print_string ")"
  | Fun(nom,e) -> aff_aux "Fun(" (Identifier nom)  e
  | App(e1, e2) -> ps "App("; affiche_expr e1;ps ", "; affiche_expr e2; ps ")"
  | PrintInt(e) -> ps "prInt("; affiche_expr e; ps ")"

        
  | Cond(b,e1,e2) ->
        print_string "Cond(";
        affiche_expr b;
	print_string ", ";
	affiche_expr e1;
	print_string ", ";
	affiche_expr e2;
	print_string ")"
   | Testeq(e1,e2) -> aff_aux "Testeq(" e1 e2
   | Testneq(e1,e2) -> aff_aux "Testneq(" e1 e2
   | Testlt(e1,e2) -> aff_aux "Testlt(" e1 e2
   | Testgt(e1,e2) -> aff_aux "Testgt(" e1 e2
   | Testlet(e1,e2) -> aff_aux "Testlet(" e1 e2
   | Testget(e1,e2) -> aff_aux "Testget(" e1 e2

   |_ -> ps "A traiter"
;;

let debug e env =
  if !debugmode then (print_env env);; 

exception EE of string;;

(* Renvoie les variables libres d'une expression *)
let rec freevars bindedvars fvars = function
  |Identifier x when (VarsSet.mem x bindedvars == false) -> VarsSet.add x fvars
  |Identifier x -> fvars
  | PrintInt e -> freevars bindedvars fvars e
  (* | Seq(e1,e2) -> eval e1 env; *)

  | Let(nom, e1, e2) -> VarsSet.union (freevars bindedvars fvars e1) (freevars (VarsSet.add nom bindedvars) fvars e2)
  | Cond(booleen,e1,e2) -> VarsSet.union (VarsSet.union (freevars bindedvars fvars e1) (freevars bindedvars fvars e2)) (freevars bindedvars fvars booleen) 

  |Fun(nom, expr) -> freevars (VarsSet.add nom bindedvars) fvars expr 


  | Add(e1,e2) 
  | Mul(e1,e2) 
  | Sou(e1,e2) 
  | Div(e1,e2) 
  | App(e1, e2)
  | Testeq(e1,e2) 
  | Testneq(e1,e2)
  | Testlt(e1,e2) 
  | Testgt(e1,e2)
  | Testlet(e1,e2)
  | Testget(e1,e2) -> VarsSet.union (freevars bindedvars fvars e1) (freevars bindedvars fvars e2)
;;

 
  

(* sémantique opérationnelle à grands pas *)
(*modifions le type de cette fonction, désormais eval -> expr -> env -> value*)
let rec eval e env  =
  debug e env;

    match e with
    | Const k -> Int k
    | Identifier k -> try Environnement.find k env with
                      | Not_found -> print_string k; 0
    | PrintInt e -> let Int x = (eval e env) in Int (prInt x)
    (* | Seq(e1,e2) -> eval e1 env; *)
    | Add(e1,e2) -> safe_add (eval e1 env) (eval e2 env)
    | Mul(e1,e2) -> safe_mult (eval e1 env) (eval e2 env)
    | Sou(e1,e2) -> safe_sou (eval e1 env) (eval e2 env)
    | Div(e1,e2) -> safe_div (eval e1 env) (eval e2 env)  
    | Let(nom, e1, e2) -> evallet e  nom e1 e2 env
    | Cond(booleen,e1,e2) -> let b = (evalb booleen env) in  if b  then (eval e1 env) else (eval e2 env) (*il me semble que c'est ainsi qu'on va gérer les booléens*)

    |Fun(nom, expr) -> Fonction (fun x -> (let fenv = (Environnement.add nom x env) in  eval expr fenv))
    |App(e1, e2) -> let Fonction(f) = (eval e1 env) in f (eval e2 env)

         
 and evalb e env = match e with
  | Testeq(e1,e2) ->  safe_op (eval e1 env) (=) (eval e2 env)  
  | Testneq(e1,e2) -> safe_op (eval e1 env) (<>) (eval e2 env)
  | Testlt(e1,e2) ->  safe_op (eval e1 env) (<) (eval e2 env)
  | Testgt(e1,e2) ->  safe_op (eval e1 env) (>) (eval e2 env)
  | Testlet(e1,e2) -> safe_op (eval e1 env) (<=) (eval e2 env)
  | Testget(e1,e2) -> safe_op (eval e1 env) (>=) (eval e2 env)

  and evallet e nom e1 e2 env = match nom with
                         |"_" -> eval e1 env; eval e2 env
                         |_ -> let envir = Environnement.add nom (eval e1 env) env in eval e2 envir

;;
  
