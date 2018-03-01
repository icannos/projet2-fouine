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
;;

let debug e env =
  if !debugmode then (print_env env);; 

exception EE of string;;

(* sémantique opérationnelle à grands pas *)
(*modifions le type de cette fonction, désormais eval -> expr -> env -> value*)
let rec eval e env  =
  debug e env;

  try
    match e with
    | Const k -> debug e env; k
    | Identifier k -> Environnement.find k env
    | PrintInt e -> prInt (eval e env)
    (* | Seq(e1,e2) -> eval e1 env; *)
    | Add(e1,e2) -> (eval e1 env) + (eval e2 env)
    | Mul(e1,e2) -> (eval e1 env) * (eval e2 env)
    | Sou(e1,e2) -> (eval e1 env) - (eval e2 env)
    | Div(e1,e2) -> let d = (eval e2 env) in  if (d <> 0) then ( (eval e1 env) / d ) else ( failwith "Div 0" )  
    | Let(nom, e1, e2) -> evallet e  nom e1 e2 env
    | Cond(booleen,e1,e2) -> if (evalb booleen env) then (eval e1 env) else (eval e2 env) (*il me semble que c'est ainsi qu'on va gérer les booléens*)
  with
  | _ -> let start_pos = Parsing.rhs_start_pos 5 in
         ps "Error at line:"; print_int start_pos.pos_lnum; 0
         
 and evalb e env = match e with
  | Testeq(e1,e2) -> (eval e1 env) = (eval e2 env)  
  | Testneq(e1,e2) -> (eval e1 env) <> (eval e2 env)
  | Testlt(e1,e2) -> (eval e1 env) < (eval e2 env)
  | Testgt(e1,e2) -> (eval e1 env) > (eval e2 env)
  | Testlet(e1,e2) -> (eval e1 env) <= (eval e2 env)
  | Testget(e1,e2) -> (eval e1 env) >= (eval e2 env)

  and evallet e nom e1 e2 env = match nom with
                         |"_" -> eval e1 env; eval e2 env
                         |_ -> let envir = Environnement.add nom (eval e1 env) env in eval e2 envir
;;
  
