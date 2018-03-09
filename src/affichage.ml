open Display;;
open Env;;
open Expr;;
open Arguments;;


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
let rec affiche_expr e = match e with
  | Identifier s -> ps s
  | Const k -> print_int k
  | Add(e1,e2) -> aff_aux "Add(" e1 e2
  | Mul(e1,e2) -> aff_aux "Mul(" e1 e2
  | Sou(e1,e2) -> aff_aux "Sou(" e1 e2
  | Div(e1,e2) -> aff_aux "Div(" e1 e2
  | Aff(nom, e) -> ps nom; ps " := "; affiche_expr e
  | Ref(e) -> ps "ref " ; affiche_expr e
  | Acc(nom) -> ps "!"; ps nom
  | Let((nom,e1),e2) ->
        print_string "Let(";
        print_string nom;
	print_string ", ";
	affiche_expr e1;
	print_string ", ";
	affiche_expr e2;
	print_string ")"
  | LetRec((nom,e1),e2) ->
        print_string "LetRec(";
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
        affiche_bexpr b;
	print_string ", ";
	affiche_expr e1;
	print_string ", ";
	affiche_expr e2;
	print_string ")"

      
and affiche_bexpr b = match b with
   | Testeq(e1,e2) -> aff_aux "Testeq(" e1 e2
   | Testneq(e1,e2) -> aff_aux "Testneq(" e1 e2
   | Testlt(e1,e2) -> aff_aux "Testlt(" e1 e2
   | Testgt(e1,e2) -> aff_aux "Testgt(" e1 e2
   | Testlet(e1,e2) -> aff_aux "Testlet(" e1 e2
   | Testget(e1,e2) -> aff_aux "Testget(" e1 e2
and  aff_aux s a b = 
      begin
	print_string s;
	affiche_expr a;
	print_string ", ";
	affiche_expr b;
	print_string ")"
      end

;;



let print_env env = ps "{"; Environnement.iter penv_item env; ps "} \n";;


let debug e env =
  if !debugmode then (print_env env);; 
