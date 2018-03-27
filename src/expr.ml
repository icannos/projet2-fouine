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
  | LetRec of  (name* extexpr)* extexpr
  | Identifier of name
  | Fun of name * extexpr
  | App of extexpr * extexpr

  (* Pattern Matching  *)
  |Cart of extexpr list
  |Constr of name * extexpr list
  |Match of extexpr * extexpr list
  |PattCase of extexpr * extexpr

  (* Built in *)
  |PrintInt of extexpr

  (* Imp�ratif *)
  |Aff of name * extexpr
  |Ref of extexpr
  |Acc of extexpr

  |Uni

  (*Liste*)
  |Vide
  |Liste of extexpr * extexpr

  (* Tests Constructor *)
  |Cond of bextexpr * extexpr * extexpr

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
  let (_, e) = expr in
  match e with
  |Identifier x ->VarsSet.singleton x
  |Cart listxpr
   |Constr(_, listxpr) ->  List.fold_right VarsSet.union (List.map getIdentifiersInConstr listxpr) VarsSet.empty
  |Const _ -> VarsSet.empty
  |Vide -> VarsSet.empty
  |Liste(ex, (_,Identifier x)) -> VarsSet.union (getIdentifiersInConstr ex) (VarsSet.singleton x)
  |Liste(ex, l) -> VarsSet.union (getIdentifiersInConstr ex) (getIdentifiersInConstr l)
  |_ -> failwith "Cannot unify"
;;

(* Renvoie les variables libres d'une expression *)
(*type de la fonction : set -> set -> expr-> set, donc il faut modifier les tests bool�ens, dis moi si cette technique te semble correcte*)
let rec freevars bindedvars fvars ee = let (node_id, e) = ee in
  match e with
  |Aff(_, e) |Ref e -> (freevars bindedvars fvars e)
  |Acc (_)
   |Const (_) -> fvars
  |Constr (_, listxpr)
  |Cart listxpr ->
    List.fold_right VarsSet.union (List.map (freevars bindedvars fvars) listxpr) VarsSet.empty
  |Identifier x when (VarsSet.mem x bindedvars == false) -> VarsSet.add x fvars
  |Uni
  |Vide
  |Identifier _-> fvars
  |Liste(t,q)-> VarsSet.union (freevars bindedvars fvars t) (freevars bindedvars fvars q)
  | PrintInt e -> freevars bindedvars fvars e
  | Let((constr_expr, e1), e2) ->
     VarsSet.union (freevars bindedvars fvars e1) (freevars (VarsSet.union (getIdentifiersInConstr constr_expr) bindedvars) fvars e2)
  | LetRec((nom, e1), e2) ->
     VarsSet.union (freevars bindedvars  fvars e1)(freevars (VarsSet.add nom bindedvars) fvars e2)
  | Cond(booleen,e1,e2) -> VarsSet.union (VarsSet.union (freevars bindedvars fvars e1) (freevars bindedvars fvars e2)) (freevarsb bindedvars fvars booleen)

  |Fun(nom, expr) -> freevars (VarsSet.add nom bindedvars) fvars expr

  |Match(expr, listxpr) ->  List.fold_right VarsSet.union (List.map (freevars bindedvars fvars) listxpr) (freevars bindedvars fvars  expr)
  |PattCase(casexpr, expr) ->freevars (VarsSet.union bindedvars (getIdentifiersInConstr casexpr)) fvars expr

  | Add(e1,e2)
  | Mul(e1,e2)
  | Sou(e1,e2)
  | Div(e1,e2)
  | App(e1, e2) -> VarsSet.union (freevars bindedvars fvars e1) (freevars bindedvars fvars e2)
and freevarsb bindedvars fvars ee =
  let (_, e) = ee in
  match e with
  | Testeq(e1,e2)
  | Testneq(e1,e2)
  | Testlt(e1,e2)
  | Testgt(e1,e2)
  | Testlet(e1,e2)
  | Testget(e1,e2) -> VarsSet.union (freevars bindedvars fvars e1) (freevars bindedvars fvars e2)

;;
