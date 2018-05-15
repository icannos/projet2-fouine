open Expr
open Env
open Errmgr
open Display

module EnvType = Map.Make(String);;


type f_type = Int_f
| List_f of f_type (* On stocke le type d'élément stockés dans la liste *)
| Fun_f of f_type * f_type (* Une fonction fouine, de type truc donne bidule *)
| Cart_f of f_type list (* Un produit cart c'est une liste de type*)
| UserType of string (* Nom du type somme défini par le programmeur *)
| Free of string (* Pour stocker des trucs par encore typé *)
| Ref_f of f_type (* Si on est une ref on connait le type du truc pointé*)
| Unit_t
;;

(* Convertit un objet type en une chaîne lisible *)
let rec string_of_ftype type_list = function
|Int_f -> "int"
|Fun_f(t1,t2) -> (string_of_ftype type_list t1) ^ " -> " ^ (string_of_ftype type_list t2)
|List_f t -> (string_of_ftype type_list t) ^ " list"
|Cart_f l -> join " * " (List.map (string_of_ftype type_list) l)
|UserType s -> s ^ " (aka "^ string_of_ftype type_list (EnvType.find s type_list) ^ " )"
|Free s -> s
|Ref_f t -> "ref " ^ string_of_ftype type_list t
|Unit_t -> "unit"
;;

let print_envtype type_list k t = ps (k ^ ": " ^ (string_of_ftype type_list t) ^" ");;

(* On stocke les types définis par l'utilisateur *)
type type_list_t = f_type EnvType.t;;

let updatevar env x v = env := EnvType.add x v !env;;
let getvartype env x = try EnvType.find x !env
        with _ -> env := (EnvType.add x (Free(x)) (!env)); Free x
;;

(* Pour stocker le type des variables *)
type env_type_t = f_type EnvType.t;;

(* Récupère les types des variables pour les sauvergarder lorsqu'on les écrase*)
let save_vars patt env =
    let ids_saved = ref [] in

    let save_and_reset x = if EnvType.mem x !env then
    ids_saved := (x, EnvType.find x !env)::!ids_saved; env := EnvType.remove x !env
    in
    VarsSet.iter save_and_reset (getIdentifiersInConstr patt); !ids_saved

;;

let rec setback_vars env = function
|[] -> ()
|(x,t)::q -> env := EnvType.add x t !env; setback_vars env q
;;


(* Décide si deux trucs peuvent avoir même type, et si oui renvoie le type
convenant aux deux trucs, sinon explose*)
let t_unify (e_t1 : f_type) (e_t2 : f_type) (env : env_type_t ref) (type_list : type_list_t) =
  let rec unif e_t1 e_t2 =
    match e_t1, e_t2 with
    |x, y when x = y -> x
    |Free a, Free b -> updatevar env a (Free a);updatevar env b (Free a); Free a
    |Free a, x |x, Free a -> updatevar env a x;x
    |Fun_f(x1, y1), Fun_f(x2, y2) -> Fun_f(unif x1 x2 , unif y1 y2)
    |Ref_f(x1), Ref_f(x2) -> Ref_f(unif x1 x2)
    |List_f(x1), List_f(x2) -> List_f (unif x1 x2)
    |UserType s, x |x, UserType s -> unif (EnvType.find s type_list) x
    |Cart_f l1, Cart_f l2 when (List.length l1 = List.length l2) -> Cart_f (List.map2 unif l1 l2)

    | _ -> raise (TypesDoNotMatch (string_of_ftype type_list e_t1, string_of_ftype type_list e_t2))
  in unif e_t1 e_t2
;;

let rec infer ee (env : env_type_t) (type_list : type_list_t) =
  let (node_id, e) = ee in (* Le node_id est important ici pour râler concernant
    les types qui matchent pas *)

    try
  match e with
  | Const x -> Int_f, env
  | Identifier (x,_) -> let env = ref env in (getvartype env x, !env)
  | Vide -> List_f (Free "'a"), env
  | Fun(patt, expr) ->
  let env = ref env in
  let svars = save_vars patt env in
  let (expr_t, envir) = infer expr !env type_list in
  let (patt_t, envir) = infer patt envir type_list in
  let env = ref envir in setback_vars env svars;
      (Fun_f(patt_t, expr_t), !env)
  | Add(e1, e2)
  | Sou(e1, e2)
  | Mul(e1, e2)
  | Div(e1, e2) ->
  (* traitement de e1 *)
    let (e1_t, envir) = infer e1 env type_list in
    let env = ref envir in let e1_t = t_unify e1_t Int_f env type_list in
  (* traitement de e2, ayant déjà les connaissances issues de e1 *)
    let (e2_t, envir) = infer e2 !env type_list in
    let env = ref envir in let e2_t = t_unify e2_t Int_f env type_list in
          (Int_f, !env)

  |App(f, e) -> begin match infer f env type_list with
                |Fun_f(patt_t, expr_t), envir ->
                  let (e_t, envir) = infer e env type_list in
                  let env = ref envir in
                  let _ = t_unify e_t patt_t env type_list in
                  (expr_t , !env)
                |x, _-> raise (NotFunction (string_of_ftype type_list x))
                end

  |Let((patt, e1), e2) ->
  let env = ref env in
  let svars = save_vars patt env in
  let (e1_t, envir) = infer e1 !env type_list in

  let (patt_t, envir) = infer patt envir type_list in
  let env = ref envir in let _ = t_unify e1_t patt_t env type_list in
  infer e2 !env type_list

  |Ref(e) ->   let (e_t, envir) = infer e env type_list in (Ref_f e_t, envir)
  |Acc(e) -> let (e_t, envir) = infer e env type_list in
        begin match e_t with
        |Ref_f t -> (t, envir)
        |x -> raise (TypesDoNotMatch (string_of_ftype type_list x, "'a ref" ))
        end
  |Aff(e1, e2) -> let (e1_t, envir) = infer e1 env type_list in
                  let (e2_t, envir) = infer e2 env type_list in
        begin match e1_t with
        |Ref_f t -> let env = ref envir in
        let _ = t_unify t e2_t env type_list in (Unit_t, !env)
        |x -> raise (TypesDoNotMatch
          (string_of_ftype type_list x, string_of_ftype type_list (Ref_f e2_t) ))
        end

  |PrintInt e -> let (e_t, envir) = infer e env type_list in
  let env = ref envir in let e_t = t_unify e_t Int_f env type_list in
    (Int_f, !env)

  |Cond((_, Testeq(e1,e2)), e3, e4)
  |Cond((_,Testneq(e1,e2)), e3, e4)
  |Cond((_,Testlt(e1,e2)), e3, e4)
  |Cond((_,Testgt(e1,e2)), e3, e4)
  |Cond((_,Testlet(e1,e2)), e3, e4)
  |Cond((_,Testget(e1,e2)), e3, e4) ->
  (* On ne compare que des entiers *)
  let (e1_t, envir) = infer e1 env type_list in
  let env = ref envir in let e1_t = t_unify e1_t Int_f env type_list in
  let (e2_t, envir) = infer e2 !env type_list in
  let env = ref envir in let e2_t = t_unify e2_t Int_f env type_list in
  let (e3_t, envir) = infer e3 !env type_list in
  let (e4_t, envir) = infer e4 envir type_list in
  let env = ref envir in
  let connmon_t = t_unify e4_t e3_t env type_list in (connmon_t, !env)

  |Cart l ->
  let rec typelist env type_list =
      begin function
      |[] -> [], env
      |x::q -> let (x_t, envir) = infer x env type_list in
               let (ctype, envir) = typelist envir type_list q in
               x_t::ctype, envir
      end in
      let ctype1, envir = typelist env type_list l in
      let ctype2, envir = typelist envir type_list l in
      let env = ref envir in let ctype = t_unify (Cart_f ctype2) (Cart_f ctype1) env type_list in
          ctype, !env






  with x -> error_display node_id x
  ;;
