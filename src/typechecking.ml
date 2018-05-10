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
;;

(* Convertit un objet type en une chaîne lisible *)
let rec string_of_ftype type_list = function
|Int_f -> "int"
|Fun_f(t1,t2) -> (string_of_ftype type_list t1) ^ " -> " ^ (string_of_ftype type_list t2)
|List_f t -> (string_of_ftype type_list t) ^ " list"
|Cart_f l -> join "*" (List.map (string_of_ftype type_list) l)
|UserType s -> s ^ " (aka "^ string_of_ftype type_list (EnvType.find s type_list) ^ " )"
|Free s -> s
|Ref_f t -> "ref " ^ string_of_ftype type_list t
;;

let print_envtype type_list k t = ps (k ^ ": " ^ (string_of_ftype type_list t) ^" ");;

(* On stocke les types définis par l'utilisateur *)
type type_list_t = f_type EnvType.t;;

let updatevar env x v = env := EnvType.add x v !env;;
let getvartype env x =
try EnvType.find x !env with _ -> env := (EnvType.add x (Free(x)) (!env)); Free x
;;

(* Pour stocker le type des variables *)
type env_type_t = f_type EnvType.t;;

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

    | _ -> raise (TypesDoNotMatch (string_of_ftype type_list e_t1, string_of_ftype type_list e_t1))
  in unif e_t1 e_t2
;;

let rec infer ee (env : env_type_t) (type_list : type_list_t) =
  let (node_id, e) = ee in (* Le node_id est important ici pour râler concernant
    les types qui matchent pas *)
  match e with
  | Const x -> Int_f, env
  | Identifier (x,_) -> let env = ref env in (getvartype env x, !env)
  | Vide -> List_f (Free "'a"), env
  | Fun(patt, expr) -> let (expr_t, envir) = infer expr env type_list in
  let (patt_t, envir) = infer patt envir type_list in (Fun_f(patt_t, expr_t), envir)
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



  ;;
