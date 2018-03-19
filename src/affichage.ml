open Display;;
open Env;;
open Expr;;
open Arguments;;

(*nouvelle fonction d'affichage, le but est de sortir un code executable *)
let rec aff_expr ee =
  let (node_id, e) = ee in
  match e with
  | Identifier s -> ps s
  | Const k -> print_int k
  | Add(e1,e2) -> aff_bin "+" e1 e2
  | Mul(e1,e2) -> aff_bin "*" e1 e2
  | Sou(e1,e2) -> aff_bin "-" e1 e2
  | Div(e1,e2) -> aff_bin "/" e1 e2
  | Aff(nom,e1) -> ps nom; ps " := "; aff_expr e1
  | Ref(e) -> ps " ref " ; aff_expr e
  | Acc(nom) -> ps " !"; ps nom
  | PrintInt(e) -> ps "printInt (" ; aff_expr e ; ps ")"
  | Let((patt,e1),e2) ->
        ps "let "; aff_expr patt; ps " = ";
	aff_expr e1;
	ps " in  "; aff_expr e2;
  | LetRec((nom,e1),e2) ->
        ps "let rec "; ps nom; ps " = ";
	aff_expr e1;
	ps " in  "; aff_expr e2;
  | Fun(nom,e1) -> ps  "( fun "; aff_expr (node_id, Identifier nom) ; ps " -> "; aff_expr e1 ; ps " )"
  | App(e1,e2) -> ps "("; aff_expr e1;ps " " ; aff_expr e2; ps ")"
  | Cond(b,e1,e2) -> ps "if "; aff_bexpr b; ps " then ( ";aff_expr e1;ps ") else (" ; aff_expr e2; ps ")"
and aff_bexpr bb=
  let (node_id, b) = bb in
  match b with
  | Testeq(e1,e2)-> aff_bin "=" e1 e2
  | Testneq(e1,e2) -> aff_bin "<>" e1 e2
  | Testlt(e1,e2) -> aff_bin "<" e1 e2
  | Testgt(e1,e2) -> aff_bin ">" e1 e2
  | Testlet(e1,e2) -> aff_bin "<=" e1 e2
  | Testget(e1,e2) -> aff_bin ">=" e1 e2

and aff_bin op a b =
  begin
    ps "(";
    aff_expr a;
    ps " ";
    ps op;
    ps " ";
    aff_expr b;
    ps ")"
  end ;;
    
  
(* fonction d'affichage des expressions *)
let rec affiche_expr ee =
let (node_id, e) = ee in
  match e with
  | Identifier s -> ps s
  | Const k -> print_int k
  | Add(e1,e2) -> aff_aux "Add(" e1 e2
  | Mul(e1,e2) -> aff_aux "Mul(" e1 e2
  | Sou(e1,e2) -> aff_aux "Sou(" e1 e2
  | Div(e1,e2) -> aff_aux "Div(" e1 e2
  | Aff(nom, e1) -> ps nom; ps " := "; affiche_expr e1
  | Ref(e) -> ps "ref " ; affiche_expr e
  | Acc(nom) -> ps "!"; ps nom
  | Let((patt,e1),e2) ->
        ps "Let(";
       affiche_expr patt;
	ps ", ";
	affiche_expr e1;
	ps ", ";
	affiche_expr e2;
	ps ")"
  | LetRec((nom,e1),e2) ->
        print_string "LetRec(";
        ps nom;
	print_string ", ";
	affiche_expr e1;
	print_string ", "; 
	affiche_expr e2;
	print_string ")"
  | Fun(nom,e1) -> aff_aux "Fun(" (node_id, Identifier nom)  e1
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

      
and affiche_bexpr bb =
let (node_id, b) = bb in
  match b with
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



let print_identifier = function
  |s -> print_string s
;;


let print_value = function
  |Int x -> print_int x
  |Unit -> ps "()"
  |Reference k -> ps "Reference"
  |Rec(nom, arg, expr, env) -> ps ("Recursive function " ^ nom)
  |Fonction(name, expr, env) -> ps ("Function " ^ name)
;;

let penv_item identifier v =
  print_identifier identifier; print_string " = " ; print_value v ; ps "; ";;


let print_env env = ps "{"; Environnement.iter penv_item env; ps "} \n";;


let debug e env =
  if !verbosemode then (print_env env);; 
