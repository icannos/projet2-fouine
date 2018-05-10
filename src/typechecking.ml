open Expr
open Env


type f_type = Int_f
| List_f of f_type (* On stocke le type d'élément stockés dans la liste *)
| Fun_f of f_type * f_type (* Une fonction fouine, de type truc donne bidule *)
| Cart_f of f_type list (* Un produit cart c'est une liste de type*)
| UserType of string (* Nom du type somme défini par le programmeur *)
;;

(* On stocke les types définis par l'utilisateur *)
type type_list_t = f_type Environnement.t;;

(* Pour stocker le type des variables *)
type env_type_t = f_type Environnement.t;;

let rec infer ee (type_env : env_type_t) (type_list : type_list_t) =
  let (node_id, e) = ee in (* Le node_id est important ici pour râler concernant
    les types qui matchent pas *)

  match e with
  | Const x -> Int_f
  | _ -> expr2
