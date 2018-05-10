open Expr
open Env
open Errmgr

type f_type = Int_f
| List_f of f_type (* On stocke le type d'élément stockés dans la liste *)
| Fun_f of f_type * f_type (* Une fonction fouine, de type truc donne bidule *)
| Cart_f of f_type list (* Un produit cart c'est une liste de type*)
| UserType of string (* Nom du type somme défini par le programmeur *)
| Free of string (* Pour stocker des trucs par encore typé *)
| Ref_f of f_type (* Si on est une ref on connait le type du truc pointé*)
;;

(* On stocke les types définis par l'utilisateur *)
type type_list_t = f_type Environnement.t;;

(* Pour stocker le type des variables *)
type env_type_t = f_type Environnement.t;;

(* Décide si deux trucs peuvent avoir même type, et si oui renvoie le type
convenant aux deux trucs, sinon explose*)
let rec unify (e_t1 : f_type) (e_t2 : f_type) (type_list : type_list_t) =
  let unify = fun x y -> unify x y type_list in
  match e_t1, e_t2 with
  |Free a, Free b -> Free a
  |Free a, x |x, Free a -> x
  |Fun_f(x1, y1), Fun_f(x2, y2) -> Fun_f(unify x1 x2 , unify y1 y2)
  |Ref_f(x1), Ref_f(x2) -> Ref_f(unify x1 x2)
  |List_f(x1), List_f(x2) -> List_f (unify x1 x2)
  |UserType s, x |x, UserType s -> unify (type_list s) x
  |Cart l1, Cart l2 when (List.length l1 = List.length l2) -> Cart (List.map2 unify l1 l2)

  | _ -> raise (TypesDoNotMatch (string_of_type e_t1, string_of_type e_t1)

;;

let rec infer ee (type_env : env_type_t) (type_list : type_list_t) =
  let (node_id, e) = ee in (* Le node_id est important ici pour râler concernant
    les types qui matchent pas *)
  match e with
  | Const x -> Int_f
  | Vide -> List_f (Free "'a'")


  ;;
