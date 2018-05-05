open Expr;;
open Errmgr;;
open Composantmachine;;



let rec compile ee =
  (*prend un arbre de fouine pur et renvoie le code associé*)
  let (node_id, e) = ee in
  try match  e with
  | Const k -> [C k]
  | Add(e1,e2) -> (compile e2)@(compile e1)@[Add]
  | Mul(e1,e2) -> (compile e2)@(compile e1)@[Mul]
  | Sou(e1,e2) -> (compile e2)@(compile e1)@[Sub]
  | Div(e1,e2) -> (compile e2)@(compile e1)@[Div]
  | Let(((_, Identifier (nom,_)),e1),e2) ->  (compile e1)@[Let nom]@(compile e2)@[Endlet]
  | Identifier (x, _) -> [Access x]
  | Fun((_, Identifier (nom,_)) , expr) ->  [Clos(nom, ((compile expr)@[Ret]))]
  | LetRec(((_, Identifier (f,_)), e1), e2) -> (compile e1)@[Rec f]@[Let f]@(compile e2)@[Endlet]
  | App(e1,e2) -> (compile e2)@(compile e1)@[Apply]
  | Cond(b, e1, e2) -> (compileb b)@([IfThenElse(compile e1, compile e2)])
  | PrintInt e -> (compile e)@[Print]

  (*Aspects impératifs*)
  | Acc e -> (compile e)@[Bang]
  | Aff(expr_ref, e) -> (compile expr_ref)@(compile e)@[Aff]
  | Ref e -> (compile e)@[Ref]
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


let rec val_env env x = match env with
  | [] -> raise Not_found
  | (a,b)::q when a = x -> b
  | _::q -> val_env q x




let rec exec_code c env pile =  match (c, env, pile) with
  | ([], _, [I x]) ->(*print_string "ici ";*)  print_int x; print_newline ()
  | (Print::suitec, _, (I a)::q) -> print_string "j'affiche ";
     print_int a; print_newline () ; exec_code suitec env pile
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
  (*Aspects impératifs*)
  | (Ref::suitec, _, v::q) -> let addr = Memmachine.new_address () in
                              Memmachine.add_memory addr v;
                              exec_code suitec env ((Reference addr)::q)
  | (Bang::suitec, _, (Reference addr)::q)-> let v = Memmachine.read_address addr in exec_code suitec env (v::q)
  | (Aff::suitec, _, (Reference addr)::e::q) -> Memmachine.add_memory addr e; exec_code suitec env q


  | _ -> ( print_string "Code:"; print_newline();affiche_code c ; print_newline();
    print_string "Environnement:"; print_newline();affiche_env env; print_newline(); print_newline();
           print_string "Pile:";print_newline(); affiche_pile pile;
           print_newline(); raise Notmatched)




let execution c = exec_code c [] [];;
