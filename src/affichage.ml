open Display;;
open Env;;
open Expr;;


let print_value = function
  |Int x -> print_int x
  |_ -> raise (Invalid_argument "Cannot print a function")
;;

let print_identifier = function
  |s -> print_string s
;;

let penv_item identifier v =
  print_identifier identifier; print_string " = " ; print_value v ; ps "; ";;



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



let print_env env = ps "{"; Environnement.iter penv_item env; ps "} \n";;


let debug e env =
  if !debugmode then (print_env env);; 