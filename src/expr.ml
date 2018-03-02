(* un type pour toutes les expressions qu'on manipule *)

open Env;;
open Safe;;
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

 
  




