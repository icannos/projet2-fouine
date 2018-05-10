(* un type pour toutes les expressions qu'on manipule *)


type name = string;;
module VarsSet = Set.Make(String);;

type expr =

  (* Arith Constr *)
  | Const of int
  | Add of extexpr*extexpr
  | Mul of extexpr*extexpr
  | Sou of extexpr*extexpr
  | Div of extexpr*extexpr

  (* Binding constr  *)
  | Let of (extexpr * extexpr) * extexpr
  | LetRec of  (extexpr* extexpr)* extexpr
  | Identifier of name * extexpr
  | Fun of extexpr * extexpr
  | App of extexpr * extexpr

  (* Pattern Matching  *)
  |Cart of extexpr list
  |Constr of name * extexpr list
  |Match of extexpr * extexpr list
  |PattCase of extexpr * extexpr

  (*Exceptions*)
  | Try of extexpr * extexpr list (* Rattrapage des exceptions *)
  | Raise of extexpr (* Expression de levage d'exception *)

  (* Built in *)
  |PrintInt of extexpr

  (* Imp�ratif *)
  |Aff of extexpr * extexpr
  |Ref of extexpr
  |Acc of extexpr

  |Uni

  (*Liste*)
  |Vide
  |Liste of extexpr * extexpr

  (* Tests Constructor *)
  |Cond of bextexpr * extexpr * extexpr


  (* Types *)
  |Typed of extexpr
  |TypeId of name
  |RecordType of (name * extexpr list) * extexpr

  (* Pure *)
  |Pure of extexpr

 and bexpr =
  | Testeq of extexpr * extexpr
  | Testneq of extexpr * extexpr
  | Testlt of extexpr * extexpr
  | Testgt of extexpr * extexpr
  | Testlet of extexpr * extexpr
  | Testget of extexpr * extexpr

 and extexpr = int * expr
 and bextexpr = int * bexpr
;;



let rec getIdentifiersInConstr expr =
  (*pattern  -> VarsSet name qui correspond à l'ensemble des variables utilisées dans le pattern*)
  let (_, e) = expr in
  match e with
  |Identifier (x, _)  ->VarsSet.singleton x
  |Cart listxpr
   |Constr(_, listxpr)  ->  List.fold_right VarsSet.union (List.map getIdentifiersInConstr listxpr) VarsSet.empty
  |Const _  -> VarsSet.empty
  |Vide  -> VarsSet.empty
  |Liste(ex, (_,Identifier (x,_)))  -> VarsSet.union (getIdentifiersInConstr ex) (VarsSet.singleton x)
  |Liste(ex, l)  -> VarsSet.union (getIdentifiersInConstr ex) (getIdentifiersInConstr l)
  |_  -> raise Errmgr.FindingIdentifierFailed
;;

(* Renvoie les variables libres d'une expression *)
(*type de la fonction : set  -> set  -> expr -> set, donc il faut modifier les tests bool�ens, dis moi si cette technique te semble correcte*)
let rec freevars bindedvars fvars ee = let (node_id, e) = ee in
  match e with
  |Aff(e1, e2)  -> VarsSet.union (freevars bindedvars fvars e1) (freevars bindedvars fvars e2)
  |Ref e  -> (freevars bindedvars fvars e)
  |Acc (e)  -> (freevars bindedvars fvars e)


  |Typed _
  |TypeId _
  |Const (_)  -> fvars
  |Constr (_, listxpr)
  |Cart listxpr  ->
    List.fold_right VarsSet.union (List.map (freevars bindedvars fvars) listxpr) VarsSet.empty
  |Identifier (x, _) when (VarsSet.mem x bindedvars == false)  -> VarsSet.add x fvars
  |Uni
  |Vide
  |Identifier (_, _) -> fvars
  |Liste(t,q) -> VarsSet.union (freevars bindedvars fvars t) (freevars bindedvars fvars q)


  | Raise e
  | PrintInt e  -> freevars bindedvars fvars e
  | Let((constr_expr, e1), e2)  ->
     VarsSet.union (freevars bindedvars fvars e1) (freevars (VarsSet.union (getIdentifiersInConstr constr_expr) bindedvars) fvars e2)
  | LetRec( ((_, Identifier (nom, _)), e1), e2)  ->
     VarsSet.union (freevars bindedvars  fvars e1)(freevars (VarsSet.add nom bindedvars) fvars e2)

  | Cond(booleen,e1,e2)  -> VarsSet.union (VarsSet.union (freevars bindedvars fvars e1) (freevars bindedvars fvars e2)) (freevarsb bindedvars fvars booleen)

  |Fun(pattern, expr)  -> freevars (VarsSet.union (getIdentifiersInConstr pattern) bindedvars) fvars expr

  |Match(expr, listxpr)  ->  List.fold_right VarsSet.union (List.map (freevars bindedvars fvars) listxpr) (freevars bindedvars fvars  expr)
  |PattCase(casexpr, expr)  ->freevars (VarsSet.union bindedvars (getIdentifiersInConstr casexpr)) fvars expr

  |Try(expr, listxpr)  ->  List.fold_right VarsSet.union (List.map (freevars bindedvars fvars) listxpr) (freevars bindedvars fvars  expr)

  | Add(e1,e2)
  | Mul(e1,e2)
  | Sou(e1,e2)
  | Div(e1,e2)
  | App(e1, e2)  -> VarsSet.union (freevars bindedvars fvars e1) (freevars bindedvars fvars e2)

  |_ -> failwith "Something gone wrong with expr.freevars"
and freevarsb bindedvars fvars ee =
  let (_, e) = ee in
  match e with
  | Testeq(e1,e2)
  | Testneq(e1,e2)
  | Testlt(e1,e2)
  | Testgt(e1,e2)
  | Testlet(e1,e2)
  | Testget(e1,e2)  -> VarsSet.union (freevars bindedvars fvars e1) (freevars bindedvars fvars e2)

;;


let rec inquisition ast =
  let (node_id, e) = ast in
  match e with
  |Const x -> (0, Pure(ast))
  |Constr(nom, le) -> let lp = List.map inquisition le in
    (*if List.for_all (fun x -> match x with |(_, Pure(e)) -> true |_ -> false)) lp then  (0, Pure(ast))*)
    (node_id, Constr(nom, lp))
  |Cart le  ->let lp = List.map inquisition le in
    (*if List.for_all (fun x -> match x with |(_, Pure(e)) -> true |_ -> false)) lp then  (0, Pure(ast))*)
    (node_id, Cart(lp))

  |Add(e1,e2) -> let c = (inquisition e1,inquisition e2) in
    begin match c with
    |((_, Pure(x)), (_, Pure(y))) -> (node_id, Pure(ast))
    | (x,y) -> (node_id, Add(x,y))
    end
  |Sou(e1,e2) -> let c = (inquisition e1,inquisition e2) in
    begin match c with
    |((_, Pure(x)), (_, Pure(y))) -> (node_id, Pure(ast))
    | (x,y) -> (node_id, Sou(x,y))
    end
  |Div(e1,e2) -> let c = (inquisition e1,inquisition e2) in
    begin match c with
    |((_, Pure(x)), (_, Pure(y))) -> (node_id, Pure(ast))
    | (x,y) -> (node_id, Div(x,y))
    end
  |Mul(e1,e2) -> let c = (inquisition e1,inquisition e2) in
    begin match c with
    |((_, Pure(x)), (_, Pure(y))) -> (node_id, Pure(ast))
    | (x,y) -> (node_id, Mul(x,y))
    end

    |Let(((0, Identifier (nom, t)), e1), e2) -> let c = (inquisition e1,inquisition e2) in
      begin match c with
      |((_, Pure(x)), (_, Pure(y))) -> (node_id, Pure(ast))
      | (x,y) -> (node_id, Let(((node_id, Identifier (nom, t)), x), y))
      end

    |Let((patt, e1), e2) -> (node_id, Let((patt, inquisition e1), inquisition e2))



    |Aff(e1, e2)  -> (node_id, Aff(inquisition e1, inquisition e2))
    |Ref e  -> (node_id, Ref (inquisition e))
    |Acc (e)  -> (node_id, Acc (inquisition e))


    |Identifier (x, _) -> (node_id, Pure(ast))
    |Uni -> ast
    |Vide -> ast
    |Liste(t,q) -> (node_id, Liste(inquisition t, inquisition q))

    | Raise e -> (node_id, Raise(inquisition e))
    | PrintInt e  -> (node_id, PrintInt(inquisition e))

    | Cond(booleen,e1,e2)  -> (node_id, Cond(inquisition_bool booleen, inquisition e1, inquisition e2))
    | Fun(pattern, expr)  -> (node_id, Fun(pattern, inquisition expr))

    |Match(expr, listxpr)  ->  (node_id, Match(inquisition expr, List.map inquisition listxpr))
    |PattCase(casexpr, expr)  -> (node_id, PattCase(casexpr, inquisition expr))

    |Try(expr, listxpr)  ->  (node_id, Try(inquisition expr, List.map inquisition listxpr))

    | App(e1, e2)  -> (node_id, App(inquisition e1, inquisition e2))

    | _ -> failwith "Something gone wrong with expr.inquisition"
  and inquisition_bool ee =
    let (node_id, e) = ee in
    match e with
    | Testeq(e1,e2) -> (node_id, Testeq(inquisition e1, inquisition e2))
    | Testneq(e1,e2) -> (node_id, Testneq (inquisition e1, inquisition e2))
    | Testlt(e1,e2) -> (node_id, Testlt (inquisition e1, inquisition e2))
    | Testgt(e1,e2) -> (node_id, Testgt (inquisition e1, inquisition e2))
    | Testlet(e1,e2) -> (node_id, Testlet (inquisition e1, inquisition e2))
    | Testget(e1,e2) -> (node_id, Testget (inquisition e1, inquisition e2))
