open Expr;;
open Errmgr;;

exception Notmatched;;

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
  | Rec of name
  | Ret
  | Apply
  | IfThenElse of (instruction list) * (instruction list)
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge

type code = instruction list

type memslot = I of int
             | B of bool
             (*Pour les fonctions*)
             |Clot of (name * instruction list * environnement)
             |ClotR of (name * name * instruction list * environnement)
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
  | LetRec(((_, Identifier (f,_)), e1), e2) -> (compile e1)@[Rec f]@[Let f]@(compile e2)@[Endlet]
  | App(e1,e2) -> (compile e2)@(compile e1)@[Apply]
  | Cond(b, e1, e2) -> (compileb b)@([IfThenElse(compile e1, compile e2)])

  | _ -> failwith "Something gone wrong with tradmachine.compile."


with x -> error_display node_id x

and compileb ee =
  let node_id, e = ee in
  match e with

  | Testeq(e1,e2) -> (compile e1)@(compile e2)@[Eq]
  | Testneq(e1,e2) -> (compile e1)@(compile e2)@[Neq]
  | Testlt(e1,e2) -> (compile e1)@(compile e2)@[Lt]
  | Testgt(e1,e2) -> (compile e1)@(compile e2)@[Gt]
  | Testlet(e1,e2) -> (compile e1)@(compile e2)@[Le]
  | Testget(e1,e2) -> (compile e1)@(compile e2)@[Ge]
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

  |_ -> "Not yet implemented"
  ;;


let affiche_code e = print_string (joli_code e ""); print_newline ();;


let rec val_env env x = match env with
  | [] -> raise Not_found
  | (a,b)::q when a = x -> b
  | _::q -> val_env q x

let rec affiche_slot slot = match slot with
  |I a -> print_int a
  |Clot (nom, _,_)-> print_string nom
  |Lcode code -> print_string "Lcode"
  |Lenv env -> print_string "Lenv"
  | Eps -> print_string "Epsilon"

let rec affiche_env env = match env with
  | [] -> ()
  | (nom, _)::q -> print_string nom ; affiche_env q

let rec affiche_pile pile = match pile with
  | [] -> ()
  | memslot::q -> affiche_slot memslot; affiche_pile q


let rec exec_code c env pile =  match (c, env, pile) with
  | ([], _, [I x]) ->  print_int x; print_newline ()
  | (Add::suitec,_, (I a)::(I b)::q)  ->
     exec_code suitec env ((I (a+b))::q)
  | (Mul::suitec,_, (I a)::(I b)::q)  ->
        exec_code suitec env ((I (a*b))::q)
  | (Div::suitec,_, (I a)::(I b)::q)  ->
           exec_code suitec env ((I (a/b))::q)
  | (Sub::suitec,_, (I a)::(I b)::q)  ->
                     exec_code suitec env ((I (a-b))::q)

  |(Eq::suitec, _, u::v::q)-> exec_code suitec env ((B (u=v))::q)
  |(Neq::suitec, _, u::v::q)-> exec_code suitec env ((B (u<>v))::q)
  |(Lt::suitec, _, u::v::q)-> exec_code suitec env ((B (u<v))::q)
  |(Gt::suitec, _, u::v::q)-> exec_code suitec env ((B (u>v))::q)
  |(Le::suitec, _, u::v::q)-> exec_code suitec env ((B (u<=v))::q)
  |(Ge::suitec, _, u::v::q)-> exec_code suitec env ((B (u>=v))::q)

  |(IfThenElse(e1, e2)::suitec, _, u::q) ->
  begin
  match u with
  | B true -> exec_code (e1@suitec) env q
  | B false -> exec_code (e1@suitec) env q
  | _ -> failwith "Something gone wrong with IfThenElse in tradmachine.exec_code"
  end

  | ((C k)::suitec,_,_)  -> exec_code suitec env ((I k)::pile)
  | ((Access x)::suitec,_,_) -> let v = val_env env x in
                            exec_code suitec env (v::pile)
  | ((Let x)::suitec,_,v::q) ->
                         exec_code suitec  ((x,v)::env)  q
  | (Endlet::suitec, t::q,_) ->
                        exec_code suitec q  pile
  | (Ret::suitec,_,  v::Eps::(Lcode c1)::(Lenv e1)::q) ->
                            exec_code c1 e1 (v::q)
  | (Apply::suitec,_,(Clot(x,c1,e1))::v::q) ->
                       let newc = (x,v) in
                       exec_code c1 (newc::e1) (Eps::(Lcode suitec)::(Lenv env)::q)
  | (Apply::suitec,_,(ClotR(f,x,c1,e1))::v::q) ->
                    let newc = (x,v) and crec = (f, ClotR(f,x,c1,e1))in
                    exec_code c1 (newc::crec::e1) (Eps::(Lcode suitec)::(Lenv env)::q)
  | (Clos(x, code)::suitec,_,_)-> exec_code suitec env (Clot(x,code,env)::pile)
  | (Rec(f)::suitec,_, Clot(x,code,env)::q)-> exec_code suitec env (ClotR(f, x,code,env)::q)
  | (_,_, _) -> ( print_string "Code:"; print_newline();affiche_code c ; print_newline();
    print_string "Environnement:"; print_newline();affiche_env env; print_newline(); print_newline();
           print_string "Pile:";print_newline(); affiche_pile pile;
           print_newline(); raise Notmatched)




let execution c = exec_code c [] [];;
