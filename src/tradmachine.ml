open Expr;;
open Errmgr;;


type instruction =
  | C of int
  | Add
  | Mul
  | Sub
  | Div
  | Let of name
  | Access of name
  | Endlet

type code = instruction list

type memslot = I of int |L of  memslot list

type environnement = (name * memslot) list

type pile = memslot list


let rec compile ee =
  (*prend un arbre de fouine pur et renvoie le code associé*)
  let (node_id, e) = ee in
  try match  e with
  | Const k -> [C k]
  | Add(e1,e2) -> (compile e2)@(compile e1)@[Add]
  | Mul(e1,e2) -> (compile e2)@(compile e1)@[Mul]
  | Sou(e1,e2) -> (compile e2)@(compile e1)@[Sub]
  | Div(e1,e2) -> (compile e2)@(compile e1)@[Div]
  | Let((patt,e1),e2) -> let (_, Identifier (x,_)) = patt in (compile e1)@[Let x]@(compile e2)@[Endlet]
  | Identifier (x,_) -> [Access x]


with x -> error_display node_id x ; raise Fail
;;


let rec joli_code l s =
  match l with
  | [] -> s
  | Add::q -> joli_code q (s ^ "Add \n")
  | Mul::q -> joli_code q (s ^ "Mul \n")
  | Sub::q -> joli_code q (s ^ "Sub \n")
  | Div::q -> joli_code q (s ^ "Div \n")
  | (C k)::q -> joli_code q (s ^ "C " ^ (string_of_int k) ^ "\n")
  | (Access x)::q -> joli_code q (s ^ "Access "^ x ^ "\n")
  | (Let x)::q -> joli_code q (s ^ "Let " ^x ^"\n")
  | Endlet::q -> joli_code q (s ^ "Endlet \n")

  ;;


let affiche_code e = print_string (joli_code e ""); print_newline ();;


let rec val_env env x = match env with
  | [] -> raise Not_found
  | (a,b)::q when a = x -> b
  | _::q -> val_env q x

let miseajour inst (env, pile) = match inst with
  | Add -> let  (I a)::(I b)::q = pile in (env, (I (a+b))::q)
  | Mul -> let (I a)::(I b)::q = pile in (env,(I (a*b))::q)
  | Sub -> let (I a)::(I b)::q = pile in (env,(I (a-b))::q)
  | Div -> let (I a)::(I b)::q = pile in (env,(I (a/b))::q)
  | (C k) -> (env,(I k)::pile)
  | (Access x) -> let v = val_env env x in (env, v::pile)
  | (Let x) -> let v::q = pile in ( (x,v)::env, q)
  | Endlet -> let t::q = env in (q, pile)

let rec exec_code c env pile = match c with
  | [] -> let [I x] = pile in print_int x; print_newline ()
  | i::q -> let (newenv, newpile) = miseajour i (env, pile) in
            exec_code q newenv newpile

let execution c = exec_code c [] [];;
