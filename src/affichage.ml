open Display;;
open Env;;
open Expr;;
open Arguments;;
open Memory


let string_of_identifier = function
  |s -> s
;;



(*string_of_expr prend en entrée une expression et retourne  un code executable en Caml *)
let rec string_of_expr n ee =
  let (node_id, e) = ee in
  match e with
  | Identifier (s, _) -> s
  | Const k -> string_of_int k
  | Add(e1,e2) -> string_of_expr_bin n "+" e1 e2
  | Mul(e1,e2) -> string_of_expr_bin n "*" e1 e2
  | Sou(e1,e2) -> string_of_expr_bin n "-" e1 e2
  | Div(e1,e2) -> string_of_expr_bin n "/" e1 e2
  | Aff(e,e1) ->  (string_of_expr n e) ^ " := " ^ (string_of_expr n e1)
  | Ref(e) ->  " ref " ^ (string_of_expr n e)
  | Acc(e) ->  " !"^ (string_of_expr n e)
  | PrintInt(e) -> "prInt (" ^ (string_of_expr n e)  ^ ")"
  | Let((patt,e1),e2) ->
         "let "^ (string_of_expr n patt) ^ " = "^ (string_of_expr n e1)^ "\n" ^ (tab n) ^
	" in  "^ (string_of_expr (n+1) e2)
  | LetRec(((_, Identifier (nom, _)),e1),e2) -> "let rec " ^ nom ^ " = "^
	(string_of_expr n e1)	^ "\n" ^(tab n) ^ " in  "^ (string_of_expr (n+1) e2)
  | Fun(pattern,e1) ->   "( fun "^ (string_of_expr n  pattern)  ^ " -> " ^ (string_of_expr n e1)  ^ " )"
  | App(e1,e2) ->  "("^ (string_of_expr n e1)^ " " ^ (string_of_expr n e2) ^ ")"
  | Cond(b,e1,e2) ->  "if "^ (aff_bexpr n b)^"\n" ^ (tab n) ^ "then \n"^(tab (n+1)) ^"("^(string_of_expr (n+2) e1)^ ") \n"^(tab n) ^"else\n"^ (tab (n+1)) ^"("  ^(string_of_expr (n+2) e2)^ ")"
  | Uni ->  "()"
  | Vide ->  "[]"
  | Liste(t,q)-> (string_of_expr n t)^ "::" ^ (string_of_expr n q)
  | Cart up -> "(" ^ (join "," (List.map (string_of_expr n) up))  ^ ")"
  | Constr (construct, up) ->  "constr(" ^ construct ^ "," ^ (List.fold_right (fun s1 s2 -> if(s2 <> "") then s1 ^ "," ^ s2 else s1 ^ s2) (List.map (string_of_expr n) up) "") ^ ")"
  | Match(x,listcases) -> "(match " ^ (string_of_expr n x)  ^ " with \n" ^ (List.fold_right (^) (List.map (string_of_expr n)  listcases) "") ^ ")"
  | Try(x,listcases) ->(tab n) ^ "(try " ^ (string_of_expr (n+1)  x)  ^ "\n"^ (tab n) ^"with \n" ^ (List.fold_right (^) (List.map (string_of_expr (n+1)) listcases) "")^")"
  | PattCase(pattern, expr) -> (tab n) ^"| " ^ (string_of_expr n pattern) ^ " -> " ^ (string_of_expr n expr) ^"\n"
  | Raise x -> "raise (" ^ (string_of_expr n x) ^")"

  |_ -> failwith "Something gone wrong with affichage.string_of_expr"
and aff_bexpr n bb=
  let (node_id, b) = bb in
  match b with
  | Testeq(e1,e2)-> string_of_expr_bin n "=" e1 e2
  | Testneq(e1,e2) -> string_of_expr_bin n "<>" e1 e2
  | Testlt(e1,e2) -> string_of_expr_bin n "<" e1 e2
  | Testgt(e1,e2) -> string_of_expr_bin n ">" e1 e2
  | Testlet(e1,e2) -> string_of_expr_bin n "<=" e1 e2
  | Testget(e1,e2) -> string_of_expr_bin n ">=" e1 e2

and string_of_expr_bin n op a b =
  begin "("^ (string_of_expr n a)^  " "^  op^  " "^  (string_of_expr n  b)^   ")"  end ;;





(* fonction d'affichage des expressions *)
let rec istring_of_expr ee =
let (node_id, e) = ee in
  match e with
  | Identifier (s, _) -> "(0," ^ "Identifier \""^ s ^ "\")"
  | Const k -> "(0, Const " ^ (string_of_int k) ^")"
  | Add(e1,e2) -> istring_aux "Add(" e1 e2
  | Mul(e1,e2) -> istring_aux "Mul(" e1 e2
  | Sou(e1,e2) -> istring_aux "Sou(" e1 e2
  | Div(e1,e2) -> istring_aux "Div(" e1 e2
  | Aff(e, e1) ->  "(0,Aff(" ^ ((istring_of_expr e)) ^ ", " ^(istring_of_expr e1) ^ " ))"
  | Ref(e) -> "(0,Ref " ^ (istring_of_expr e)^")"
  | Acc(e) -> "(0,Acc"^ (istring_of_expr e)^")"
  | Let((patt,e1),e2) -> "(0,Let( ("^ istring_of_expr patt ^ ", "^ (istring_of_expr e1) ^ "), ("^ (istring_of_expr e2) 	^ ")))"
  | LetRec(((0, Identifier (nom, _)),e1),e2) -> "(0,LetRec("^ nom^ ", "^(istring_of_expr e1)^
	( ", ")^ (istring_of_expr e2)^ "))"
  | Fun(pattern,e1) ->  "(0, Fun( "^ (istring_of_expr pattern) ^ "," ^ (istring_of_expr  e1) ^ "))"
  | App(e1, e2) ->  "(0,App("^ (istring_of_expr e1)^ ", "^ (istring_of_expr e2) ^ "))"
  | PrintInt(e) ->  "(0,prInt("^ (istring_of_expr e) ^ "))"


  | Cond(b,e1,e2) ->
        "(0,Cond(" ^ (istring_of_bexpr b) ^ ", " ^(istring_of_expr e1)^", "^	(istring_of_expr e2)^ "))"
  | Uni ->  "(0,Uni)"
  | Vide ->  "(0,Vide)"
  | Liste(t,q)-> istring_aux "Liste(" t q
  | Cart up -> "(0,Cart([" ^ (join ";" (List.map istring_of_expr up) ) ^ "]))"
  | Constr(nomcons, up) ->  "(0,Constr("^ nomcons ^", ["^ (join ";" (List.map istring_of_expr up)) ^ "]))"
  | PattCase(pattern, expr) -> istring_aux "Pattcase(" pattern expr
  | Match(x,listcases)->  "(0,Match(" ^ istring_of_expr x ^ "," ^ (join "" (List.map istring_of_expr listcases)) ^ "))"
  | Try(x,listcases) -> "(0,Try(" ^ istring_of_expr x ^ ", "^ (join ";" ( List.map istring_of_expr listcases)) ^ "))"
  | Raise x ->"(0, Raise (" ^ (istring_of_expr x) ^ "))"
  | _ -> "non fait"

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
      begin"(0,"^ s ^ (istring_of_expr a) ^ ", " ^	(istring_of_expr b)^	 "))"   end

;;

let rec string_of_value = function
  |Int x -> string_of_int x
  |Unit -> "()"
  |Reference k -> "Reference on " ^ (string_of_value (read_address k))
  |Rec(nom, arg, expr, env) -> ("Recursive function " ^ nom)
  |Fonction(pattern, expr, env) -> ("Function: " ^ (string_of_expr 0 pattern) ^ "->" ^ "...")
  |LVide -> "[]"
  |TSum(a,b) -> a ^ "(" ^ (join "," (List.map string_of_value b)) ^ ")"
  |Cartesian x-> "("^ (join "," (List.map string_of_value x)) ^ ")"
  |Listing(a,b)-> (string_of_value a) ^ "::" ^ (string_of_value b)
  |Bool true -> "true"
  |Bool false -> "false"
  |Exn x -> "Exception: " ^ (string_of_value x)

;;

let print_value v = ps (string_of_value v);;

let penv_item identifier v =
  ps ((string_of_identifier identifier) ^ " = " ^ (string_of_value v) ^ "; ");;


let print_env env = ps"{"; Environnement.iter penv_item env; ps "} \n";;


let debug e env =
  if !verbosemode then (print_env env);;


let aff_expr e = ps (string_of_expr 0 e);;
