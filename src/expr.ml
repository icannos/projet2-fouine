(* un type pour des expressions arithm�tiques simples *)
type expr =
    Const of int
  | Add of expr*expr
  | Mul of expr*expr




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

(* s�mantique op�rationnelle � grands pas *)
let rec eval = function
  | Const k -> k
  | Add(e1,e2) -> (eval e1) + (eval e2)
  | Mul(e1,e2) -> (eval e1) * (eval e2)

  
