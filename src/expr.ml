(* un type pour toutes les expressions qu'on manipule *)


type name = String.t;;

type expr =

  (* Arith Constr *)
  | Const of int
  | Add of expr*expr
  | Mul of expr*expr
  | Sou of expr*expr
  | Div of expr*expr

  (* Binding constr  *)
  | Let of name * expr * expr
  | Name of name


  (* Tests Constructor *)
  |Cond of expr * expr * expr
          
  | Testeq of expr * expr
  | Testneq of expr * expr
  | Testlt of expr * expr
  | Testgt of expr * expr
  | Testlet of expr * expr
  | Testget of expr * expr
;;


 module Environnement = Map.Make(String);; (*création des nos superbes environnements :dire que c'est une string doit suffire, un raffinement possible en définissant le type précis des noms de variables*)
         
 (*cette double définition ne marche pas, or je ne vois pas comment supprimer cette intrication*)
 (*je crois qu'en fait value ne doit être que int, et qu'il faut gérer la cloture autrement *) 
type value =
  | Int of int
       (*  | Cloture of fun * env *) (*J'ai commenté ce morceau car en vrai je ne sais pas si le type fun est approprié ce sera une question à poser vendredi *)
(*qui sont soit entières soit des fonctions, définies avec leur vieil environnement*)
;;
type env = value Environnement.t;; (*ces environnements contiennent des value*)

                 
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


(* sémantique opérationnelle à grands pas *)
(*modifions le type de cette fonction, désormais eval -> expr -> env -> value*)
let rec eval e env  = match e  with
  | Const k -> k
  | Add(e1,e2) -> (eval e1 env) + (eval e2 env)
  | Mul(e1,e2) -> (eval e1 env) * (eval e2 env)
  | Sou(e1,e2) -> (eval e1 env) - (eval e2 env)
  | Div(e1,e2) -> (eval e1 env) / (eval e2 env)
  | Let(nom, e1, e2) -> let envir = Environnement.add nom (eval e1 env) env in
                        eval e2 envir
  | Cond(booleen,e1,e2) -> if (evalb booleen env) then (eval e1 env) else (eval e2 env) (*il me semble que c'est ainsi qu'on va gérer les booléens*)
 and evalb e env = match e with
   | Testeq(e1,e2) -> (eval e1 env) = (eval e2 env)  
 (* | Testneq(e1,e2) -> (eval e1 env) <> (eval e2 env)
  | Testlt(e1,e2) -> (eval e1 env) < (eval e2 env)
  | Testgt(e1,e2) -> (eval e1 env) > (eval e2 env)
  | Testlet(e1,e2) -> (eval e1 env) <= (eval e2 env)
  | Testget(e1,e2) -> (eval e1 env) >= (eval e2 env)*)

  
