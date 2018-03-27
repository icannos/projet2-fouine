open Display;;
open Env;;
open Expr;;
open Arguments;;

(*string_of_expr prend en entrée une expression et retourne  un code executable en Caml *)
let rec string_of_expr ee =
  let (node_id, e) = ee in
  match e with
  | Identifier s -> s
  | Const k -> string_of_int k
  | Add(e1,e2) -> string_of_expr_bin "+" e1 e2
  | Mul(e1,e2) -> string_of_expr_bin "*" e1 e2
  | Sou(e1,e2) -> string_of_expr_bin "-" e1 e2
  | Div(e1,e2) -> string_of_expr_bin "/" e1 e2
  | Aff(nom,e1) ->  nom ^ " := " ^ (string_of_expr e1)
  | Ref(e) ->  " ref " ^ (string_of_expr e)
  | Acc(e) ->  " !"^ (string_of_expr e)
  | PrintInt(e) -> "prInt (" ^ (string_of_expr e)  ^ ")"
  | Let((patt,e1),e2) ->
         "let "^ (string_of_expr patt) ^ " = "^ (string_of_expr e1)^
	" in  "^ string_of_expr e2
  | LetRec((nom,e1),e2) -> "let rec " ^ nom ^ " = "^
	string_of_expr e1	^ " in  "^ (string_of_expr e2)
  | Fun(nom,e1) ->   "( fun "^ (string_of_expr (node_id, Identifier nom))  ^ " -> " ^ (string_of_expr e1)  ^ " )"
  | App(e1,e2) ->  "("^ (string_of_expr e1)^ " " ^ (string_of_expr e2) ^ ")"
  | Cond(b,e1,e2) ->  "if "^ (aff_bexpr b)^  " then ( "^(string_of_expr e1)^ ") else (" ^ (string_of_expr e2)^ ")"
  | Uni ->  "()"
  | Vide ->  "[]"
  | Liste(t,q)-> (string_of_expr t)^ "::" ^ (string_of_expr q)
  | Cart up -> "(" ^ (List.fold_right (^) (List.map string_of_expr up) "")  ^ ")"
  | Constr (construct, up) ->  "constr(" ^ construct ^ "," ^ (List.fold_right (^) (List.map string_of_expr up) "") ^ ")"
  | Match(x,listcases) -> "match " ^ (string_of_expr x)  ^ " with " ^ (List.fold_right (^) (List.map string_of_expr listcases) "")
  | PattCase(pattern, expr) -> "| " ^ (string_of_expr pattern) ^ " -> " ^ (string_of_expr expr)
and aff_bexpr bb=
  let (node_id, b) = bb in
  match b with
  | Testeq(e1,e2)-> string_of_expr_bin "=" e1 e2
  | Testneq(e1,e2) -> string_of_expr_bin "<>" e1 e2
  | Testlt(e1,e2) -> string_of_expr_bin "<" e1 e2
  | Testgt(e1,e2) -> string_of_expr_bin ">" e1 e2
  | Testlet(e1,e2) -> string_of_expr_bin "<=" e1 e2
  | Testget(e1,e2) -> string_of_expr_bin ">=" e1 e2

and string_of_expr_bin op a b =
  begin "("^ (string_of_expr a)^  " "^  op^  " "^  (string_of_expr b)^   ")"  end ;;


let aff_expr e = ps (string_of_expr e);;


(* fonction d'affichage des expressions *)
let rec istring_of_expr ee =
let (node_id, e) = ee in
  match e with
  | Identifier s -> s
  | Const k -> string_of_int k
  | Add(e1,e2) -> istring_aux "Add(" e1 e2
  | Mul(e1,e2) -> istring_aux "Mul(" e1 e2
  | Sou(e1,e2) -> istring_aux "Sou(" e1 e2
  | Div(e1,e2) -> istring_aux "Div(" e1 e2
  | Aff(nom, e1) -> nom^ " := "^ istring_of_expr e1
  | Ref(e) -> "ref " ^ (istring_of_expr e)
  | Acc(e) -> "!"^ (istring_of_expr e)
  | Let((patt,e1),e2) -> "Let("^ istring_of_expr patt	^ ", "^ (istring_of_expr e1)
	^ ", "^ (istring_of_expr e2) 	^ ")"
  | LetRec((nom,e1),e2) -> "LetRec("^ nom^ ", "^(istring_of_expr e1)^
	( ", ")^ (istring_of_expr e2)^ ")"
  | Fun(nom,e1) -> istring_aux "Fun(" (node_id, Identifier nom)  e1
  | App(e1, e2) ->  "App("^ (istring_of_expr e1)^ ", "^ (istring_of_expr e2) ^ ")"
  | PrintInt(e) ->  "prInt("^ (istring_of_expr e) ^ ")"


  | Cond(b,e1,e2) ->
        "Cond(" ^ (istring_of_bexpr b) ^ ", " ^(istring_of_expr e1)^", "^	(istring_of_expr e2)^ ")"
  | Uni ->  "Uni"
  | Vide ->  "[]"
  | Liste(t,q)-> istring_aux "Liste(" t q
  | Cart up -> "Cart("^ (List.fold_right (^) (List.map istring_of_expr up) "") ^ ")"
  | Constr(nomcons, up) ->  "Const("^ (List.fold_right (^) (List.map istring_of_expr up) "") ^ ")"
  | PattCase(pattern, expr) -> istring_aux "Pattcase(" pattern expr
  | Match(x,expr)->  "non fait"

and istring_of_bexpr bb =
let (node_id, b) = bb in
  match b with
   | Testeq(e1,e2) -> istring_aux "Testeq(" e1 e2
   | Testneq(e1,e2) -> istring_aux "Testneq(" e1 e2
   | Testlt(e1,e2) -> istring_aux "Testlt(" e1 e2
   | Testgt(e1,e2) -> istring_aux "Testgt(" e1 e2
   | Testlet(e1,e2) -> istring_aux "Testlet(" e1 e2
   | Testget(e1,e2) -> istring_aux "Testget(" e1 e2
and  istring_aux s a b =
      begin s ^ (istring_of_expr a) ^ ", " ^	(istring_of_expr b)^	 ")"   end

;;

let string_of_identifier = function
  |s -> s
;;

let affiche_expr e = ps (istring_of_expr e);;

let rec string_of_value = function
  |Int x -> string_of_int x
  |Unit -> "()"
  |Reference k -> "Reference"
  |Rec(nom, arg, expr, env) -> ("Recursive function " ^ nom)
  |Fonction(name, expr, env) -> ("Function " ^ name)
  |LVide -> "[]"
  |TSum(a,b) -> a ^ "(" ^ (List.fold_right (^) (List.map string_of_value b) "") ^ ")"
  |Cartesian x-> List.fold_right (^) (List.map string_of_value x) ""
  |Listing(a,b)-> (string_of_value a) ^ "::" ^ (string_of_value b)
;;

let print_value v = ps (string_of_value v);;

let penv_item identifier v =
  ps ((string_of_identifier identifier) ^ " = " ^ (string_of_value v) ^ "; ");;


let print_env env = ps"{"; Environnement.iter penv_item env; ps "} \n";;


let debug e env =
  if !verbosemode then (print_env env);;
