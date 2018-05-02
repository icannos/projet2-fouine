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
  | Clos of name * (instruction list)
  | Ret
  | Apply

type code = instruction list

type memslot = I of int
             (*Pour les fonctions*)
             |Clot of (name * instruction list * environnement)
             |Lcode of instruction list
             |Lenv of environnement
             | Eps
and
 environnement = (name * memslot) list

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
  | Let((patt,e1),e2) -> let (_, Identifier (nom,_)) = patt in (compile e1)@[Let nom]@(compile e2)@[Endlet]
  | Identifier (x, _) -> [Access x]
  | Fun(argument, expr) -> let (_, Identifier (nom,_)) = argument in [Clos(nom, ((compile expr)@[Ret]))]
  | App(e1,e2) -> (compile e2)@(compile e1)@[Apply]


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
  | Ret::q -> joli_code q (s ^ "Ret \n")
  | Apply::q -> joli_code q (s ^ "Apply \n")
  | (Clos (x, inst))::q -> joli_code q (s ^ "Clos(" ^ x ^ (joli_code inst "\n")^ ")\n")

  ;;


let affiche_code e = print_string (joli_code e ""); print_newline ();;


let rec val_env env x = match env with
  | [] -> raise Not_found
  | (a,b)::q when a = x -> b
  | _::q -> val_env q x


let rec exec_code c env pile = match c with
  | [] -> let [I x] = pile in print_int x; print_newline ()
  | i::suitec -> match i with
            | Add -> let  (I a)::(I b)::q = pile in
                     exec_code suitec env ((I (a+b))::q)
            | Mul -> let (I a)::(I b)::q = pile in
                      exec_code suitec env ((I (a*b))::q)
            | Sub -> let (I a)::(I b)::q = pile in
                      exec_code suitec env ((I (a-b))::q)
            | Div -> let (I a)::(I b)::q = pile in
                      exec_code suitec env ((I (a/b))::q)
            | (C k) -> exec_code suitec env ((I k)::pile)
            | (Access x) -> let v = val_env env x in
                            exec_code suitec env (v::pile)
            | (Let x) -> let v::q = pile in
                         exec_code suitec  ((x,v)::env)  q
            | Endlet -> let t::q = env in
                        exec_code suitec q  pile
            | Ret -> let v::(Lcode c1)::(Lenv e1)::q = pile  in
                     exec_code c1 e1 (v::q)
            | Apply -> let v::(Clot(x,c1,e1))::q = pile in
                       let newc = (x,v) in
                       exec_code c1 (newc::e1) (Eps::(Lcode c)::(Lenv env)::q) 
            | Clos(x, code)-> exec_code suitec env (Clot(x,code,env)::pile)


            
let execution c = exec_code c [] [];;
