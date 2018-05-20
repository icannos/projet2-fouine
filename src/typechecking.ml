(** Module gérant l'inférence de type de notre interpreter *)


open Expr
open Env
open Errmgr
open Display

let free_type_counter = ref (-1);;

(**Renvoie la lettre associée à un entier entre 0 et 25*)
let intToLetter c =String.make 1 (char_of_int (c + 97));;

(**Renvoie le polymorphisme voulu pour un entier donne ie 0 renvoie a' *)
let int_to_letters n = let num = n/26 in
                       if num = 0 then  ("'"^(intToLetter n))
                       else ("'"^(intToLetter (n mod 26)) ^ (string_of_int num));;

(** Construit un nouveau type polymorphique libre *)
let new_free_type () =
free_type_counter:= 1+ !free_type_counter;
(int_to_letters !free_type_counter)
;;

(** Module utilisé pour stocker l'envi des types *)
module EnvType = Map.Make(String);;


type f_type = Int_f
| List_f of f_type (** On stocke le type d'élément stockés dans la liste *)
| Fun_f of f_type * f_type (** Une fonction fouine, de type truc donne bidule *)
| Cart_f of f_type list (** Un produit cart c'est une liste de type*)
| UserType of string (** Nom du type somme défini par le programmeur *)
| TypeOf of string (** Pour stocker des trucs par encore typés *)
| Ref_f of f_type (** Si on est une ref on connait le type du truc pointé*)
| Unit_f

and
(* Pour stocker le type des variables *)
env_type_t = f_type EnvType.t
;;

(** Convertit un objet type en une chaîne lisible *)
let rec string_of_ftype type_list = function
|Int_f -> "int"
|Fun_f(t1,t2) -> "(" ^ (string_of_ftype type_list t1) ^ " -> " ^ (string_of_ftype type_list t2) ^ ")"
|List_f t -> (string_of_ftype type_list t) ^ " list"
|Cart_f l -> "(" ^ (join " * " (List.map (string_of_ftype type_list) l)) ^ ")"
|UserType s -> s ^ " (aka "^ string_of_ftype type_list (EnvType.find s type_list) ^ " )"
|TypeOf s -> s
|Ref_f t -> string_of_ftype type_list t ^ " ref"
|Unit_f -> "unit"
;;

(** Affiche un élément de l'environnement *)
let print_envtype type_list k t = ps (k ^ ": " ^ (string_of_ftype type_list t) ^" ");;

(** Affiche l'environnement complet *)
let print_env_t type_list env = EnvType.iter (print_envtype type_list) env; ps "\n";;

(** On stocke les types définis par l'utilisateur *)
type type_list_t = f_type EnvType.t;;



(** Utilitaire pour lire les types dans l'environnement*)
let getvartype env x = try EnvType.find x !env
        with Not_found ->
        (let ptype = new_free_type () in
        let _ = env := (EnvType.add x (TypeOf(ptype)) (!env)) in
        let _ = env := (EnvType.add ptype (TypeOf(ptype)) (!env)) in
        TypeOf ptype)
;;

(** Le find de l'union find *)
let rec parent_t env x = match getvartype env x with
  | TypeOf a when a = x -> TypeOf x
  | TypeOf a -> parent_t env a
  | x -> x
;;

(** Efface l'ensemble de variables donné en entrée, et renvoie une liste de
sauvegarde contenant leurs anciennes affectations *)
let erase_vars env vars =
  let l = ref [] in
    let erase_var k =
    let ptype = new_free_type () in
      l := (k, getvartype env k)::(!l);
      env := (EnvType.add k (TypeOf(ptype)) (!env));
      env := (EnvType.add ptype (TypeOf(ptype)) (!env))
    in

    VarsSet.iter erase_var vars; !l
;;

(** Utilitaire pour mettre à jour le type d'une variable, utilisée dans l'union
de l'union find*)
let updatevar env x v = env := EnvType.add x v !env;;


(** Récupère les types des variables pour les sauvergarder lorsqu'on les écrase*)
let save_vars patt env =
    let ids_saved = ref [] in

    let save_and_reset x = if EnvType.mem x !env then
    ids_saved := (x, EnvType.find x !env)::!ids_saved; env := EnvType.remove x !env
    in
    VarsSet.iter save_and_reset (getIdentifiersInConstr patt); !ids_saved

;;
(** Remet les variables sauvegardées*)
let rec setback_vars env = function
|[] -> ()
|(x,t)::q -> env := EnvType.add x t !env; setback_vars env q
;;

(** Affecte leur type aux arguments d'une fonction lors d'une application, pour
déterminer le type de retoure de la fonction *)
let rec type_arg env = function
|Int_f -> Int_f
|Fun_f(t1,t2) -> Fun_f(type_arg env t1, type_arg env t2)
|List_f t ->List_f (type_arg env t)
|Cart_f l -> Cart_f (List.map (type_arg env) l)
|UserType s -> UserType s
|TypeOf s when EnvType.mem s !env -> let t = parent_t env s in t
|Ref_f t -> Ref_f (type_arg env t)
|Unit_f -> Unit_f
|x -> x
;;


(** Décide si deux trucs peuvent avoir même type, et si oui renvoie le type
convenant aux deux trucs, sinon explose*)
let t_unify (e_t1 : f_type) (e_t2 : f_type) (env : env_type_t ref) (type_list : type_list_t) =
  let rec unif e_t1 e_t2 =
    match e_t1, e_t2 with
    |x, y when x = y -> x
    |TypeOf a, TypeOf b ->

    (* On utilise le tye polymorphique le moins grand par ordre alphabétique*)
    if a < b then
        let _ = updatevar env a (parent_t env a) in
        let _ = updatevar env b (parent_t env a) in
        let t = (parent_t env a) in t
    else
      let _ = updatevar env a (parent_t env b) in
      let _  = updatevar env b  (parent_t env b) in
      let t =  (parent_t env b) in t

    |TypeOf a, x |x, TypeOf a -> let _ = updatevar env a x in x
    |Fun_f(x1, y1), Fun_f(x2, y2) -> Fun_f(unif x1 x2 , unif y1 y2)
    |Ref_f(x1), Ref_f(x2) -> Ref_f(unif x1 x2)
    |List_f(x1), List_f(x2) -> List_f (unif x1 x2)
    |UserType s, x |x, UserType s -> unif (EnvType.find s type_list) x
    |Cart_f l1, Cart_f l2 when (List.length l1 = List.length l2) ->
    Cart_f (List.map2 unif l1 l2)

    | _ -> raise (TypesDoNotMatch (string_of_ftype type_list e_t1, string_of_ftype type_list e_t2))
  in unif e_t1 e_t2
;;

(** Fonction principale de l'inférence de type: elle renvoie le type de l'expression
ainsi que les affectations de type à toutes les variables *)
let rec infer ee (env : env_type_t) (type_list : type_list_t) =
  let (node_id, e) = ee in (* Le node_id est important ici pour râler concernant
    les types qui matchent pas *)

    try
  match e with
  | Uni -> Unit_f, env
  | Const x -> Int_f, env
  | Identifier (x,_) -> let envir = ref env in let p_x = parent_t envir x in
  (p_x,  !envir)
  | Fun(patt, expr) ->
  let env = ref env in
  (* On sauvegarde les variables qui vont être cachées par l'argument de la fonction *)
  let svars = save_vars patt env in

  (* On détermine le type de l'expression et on essaie d'inférer le type des vars qui la composent *)
  let (expr_t, envir) = infer expr !env type_list in

  (* Une fois qu'on a une idée des types variables du corps, grâce à celui-ci, on peut décider,
  au moins en partie du type du pattern en argument *)

  let (patt_t, envir) = infer patt envir type_list in

  (* On restaure les variables qu'on avait caché le temps de traiter le corps de la fonction *)
  let env = ref envir in setback_vars env svars;
      (Fun_f(patt_t, expr_t), !env)
  | Add(e1, e2)
  | Sou(e1, e2)
  | Mul(e1, e2)
  | Div(e1, e2) ->
  (* traitement de e1 *)
    let (e1_t, envir) = infer e1 env type_list in
    (* Une fois que l'on a une idée du type de e1, on essaie de le faire correspondre à un entier*)
    let env = ref envir in let _ = t_unify e1_t Int_f env type_list in
  (* traitement de e2, ayant déjà les connaissances issues de e1 *)
    let (e2_t, envir) = infer e2 !env type_list in
    (* Une fois que l'on a une idée du type de e2, on essaie de le faire correspondre à un entier*)
    let env = ref envir in let _ = t_unify e2_t Int_f env type_list in
          (Int_f, !env) (* C'est une addition, ça renvoie un entier *)

  |App(f, e) -> begin match infer f env type_list with
  (* On essaie de trouver le type de f, ce serait cool que ce soit une fonction vu qu'on l'applique *)
                |Fun_f(patt_t, expr_t), envir ->
                (* Si c'est une fonction bah on essaie de voir si le type de l'argument de la fonction et
                le type du truc sur lequel on applique peuvent correspondre*)

                (* D'abord on infère le type du truc sur lequel on veut appliquer*)
                  let (e_t, envir) = infer e envir type_list in
                  let env = ref envir in
                  (* On essaie de matcher les 2*)
                  let _ = t_unify (Fun_f(e_t, expr_t)) (Fun_f(patt_t, expr_t)) env type_list in
                  (type_arg env expr_t , !env) (* On renvoie le type de retour de la fonction*)

                  (* Dans le cas où l'on applique une variable encore non typée, on sait
                  que ça doit être une fonction prenant en argument le type de l'expression passée*)
                  |TypeOf a, envir -> let (e_t, envir) = infer e envir type_list in
                            let env = ref envir in
                            (* On construit son type de retour qui est libre *)
                            let return_type = TypeOf (new_free_type ()) in
                            (* On fait correspondre le type encore indéfini à une fonction correspondante*)
                            let _ = t_unify (TypeOf a) (Fun_f(e_t, return_type)) env type_list in
                             (return_type, !env)


                  (* Si on connait pas on explose*)
                |x, _-> raise (NotFunction (string_of_ftype type_list x))
                end

  |Let((patt, e1), e2) ->
  (* On infère le type du pattern sans supprimer les variables de même nom
   extérieure car on peut faire let a = a in .. avec a défini avant*)
  let (e1_t, envir) = infer e1 env type_list in
  let env = ref envir in
  (* Efface de l'environnement les variables écrasées par le nouveau binding *)
  let _ = erase_vars env (getIdentifiersInConstr patt) in

  (* Maintenant qu'on sait ce qu'on affecte au pattern, on peut en déterminer le type de tous
  les identifiers *)
  let (patt_t, envir) = infer patt !env type_list in

  (* Puis on unifie ce type avec le type que l'on cherche à déconstruire*)
  let env = ref envir in
  let _ = t_unify e1_t patt_t env type_list   in

  (* Ensuite on donne le type de l'expression dans laquelle on a fait l'affectations*)
  let e2_t, envir = infer e2 !env type_list in
    (e2_t, envir)

  | LetRec(((node, Identifier (nom, x)), e1), e2) ->
    let env = ref env in
    (* On met d'abord à l'abri les variables écrasées par l'affectation de f*)
    let svars = erase_vars env (getIdentifiersInConstr (node, Identifier (nom, x))) in
    (* Ensuite on constuit le type de l'identifier de la fonction récursive *)
    let (f_t, envir) = infer (node, Identifier (nom, x)) !env type_list in
    let env = ref envir in
    (* On décide dès lors que f est de type 'a -> 'b, on va s'en servir dans le corps de
    la fonction *)
    let patt_t = TypeOf (new_free_type ()) and expr_t = TypeOf (new_free_type ()) in
    let _ = t_unify f_t (Fun_f(patt_t, expr_t)) env type_list in

    (* On infère le type du corps (et de toutes les variables qui s'y trouvent,
    en particulier on précise le type de la fonction récursive elle même)*)
    let (e1_t, envir) = infer e1 !env type_list in
    let env = ref envir in

    (* On essaie alors de faire correspondre f à son affectation réelle*)
    let _ = t_unify f_t e1_t env type_list in

    (* On décide du type de la suite et on remet en place l'ancien environnement*)
    let (e2_t, envir) = infer e2 !env type_list in
    let env = ref envir in setback_vars env svars; (e2_t, !env)

  |Ref(e) -> (* On décide le type de e et on dit que ceci est de type type de e ref*)
  let (e_t, envir) = infer e env type_list in (Ref_f e_t, envir)

  |Acc(e) ->
      (* On décide le type de e et si c'est pas une référence ou un type libre on explose*)
      let (e_t, envir) = infer e env type_list in
        begin match e_t with
        |Ref_f t -> (t, envir)
        |TypeOf s -> (*Si c'est un type libre, on obtient comme info que c'est une référence,
        sur un type libre*)
        let env = ref envir in
        let type_ref = TypeOf(new_free_type ()) in
        let _ = t_unify (TypeOf s) (Ref_f type_ref) env type_list in (type_ref, !env)

        (* Sinon explose: mais c'est rattrapé par la gestion des erreurs *)
        |x -> raise (TypesDoNotMatch (string_of_ftype type_list x, "'a ref" ))
        end

  |Aff(e1, e2) -> let (e1_t, envir) = infer e1 env type_list in
                  let (e2_t, envir) = infer e2 envir type_list in
        begin match e1_t with
        |Ref_f t -> let env = ref envir in
        let _ = t_unify t e2_t env type_list in (Unit_f, !env)
        |TypeOf s -> (*Si c'est un type libre, on obtient comme info que c'est une référence,
        sur le type de e2*)
        let env = ref envir in
        let _ = t_unify (TypeOf s) (Ref_f e2_t) env type_list in (Unit_f, !env)

        (* Sinon on explose *)
        |x -> raise (TypesDoNotMatch
          (string_of_ftype type_list x, string_of_ftype type_list (Ref_f e2_t) ))
        end

  |PrintInt e ->
  (* On essaie de trovuer le type de e puis de le faire correspondre avec int puisque
  PrInt prend un entier en argument: c'est un axiome*)
  let (e_t, envir) = infer e env type_list in
  let env = ref envir in let _ = t_unify e_t Int_f env type_list in
    (Int_f, !env)

    (* On traite toutes les conditions d'un seul coup*)
  |Cond((_, Testeq(e1,e2)), e3, e4)
  |Cond((_,Testneq(e1,e2)), e3, e4)
  |Cond((_,Testlt(e1,e2)), e3, e4)
  |Cond((_,Testgt(e1,e2)), e3, e4)
  |Cond((_,Testlet(e1,e2)), e3, e4)
  |Cond((_,Testget(e1,e2)), e3, e4) ->
  (* On ne compare que des entiers *)
  let (e1_t, envir) = infer e1 env type_list in
  let env = ref envir in let _ = t_unify e1_t Int_f env type_list in
  let (e2_t, envir) = infer e2 !env type_list in
  let env = ref envir in let _ = t_unify e2_t Int_f env type_list in

  (* On calcule ensuite les types des deux branches*)
  let (e3_t, envir) = infer e3 !env type_list in
  let (e4_t, envir) = infer e4 envir type_list in
  let env = ref envir in
  (* On essaie de les faire correspondre, si ça correspond pas on explose*)
  let connmon_t = t_unify e4_t e3_t env type_list in (connmon_t, !env)

  |Cart l ->
  (** Fonction utilitaire pour typer chaque composante du couple en ayant les connaissances
  obtenues en traitant le précédent *)
  let rec typelist env type_list =
      begin function
      |[] -> [], env
      |x::q -> let (x_t, envir) = infer x env type_list in
               let (ctype, envir) = typelist envir type_list q in
               x_t::ctype, envir
      end in
      (* On effectue 2 passes ensuite pour ne rien rater *)
      let ctype1, envir = typelist env type_list l in
      let ctype2, envir = typelist envir type_list l in
      (* On tente d'unifier ces deux passes pour faire toutes les associatiations
      identifiers - types  *)
      let env = ref envir in let ctype = t_unify (Cart_f ctype2) (Cart_f ctype1) env type_list in
          ctype, !env

  |Vide -> (List_f(TypeOf(new_free_type ())), env) (* Une liste vide a le type 'a list*)
  |Liste(e1, e2) ->
          (* On calcule le type de l'élément de tête *)
          let (e1_t, envir) = infer e1 env type_list in
                  (* On calcule le type du reste de la liste *)
                  begin match infer e2 envir type_list with
                  (* Si c'est une liste liste de type l_t, on essaie de faire correspondre le type du
                  premier élément avec *)
                  |List_f(l_t), envir -> let env = ref envir in
                    let c_t = t_unify (List_f e1_t) (List_f l_t) env type_list in c_t, !env
                  |TypeOf x, envir ->
                  (* Si la suite est un type indéfini alors le tout est une liste du type du premier élément*)
                  let env = ref envir in
                  let c_t = t_unify (List_f e1_t) (TypeOf x) env type_list in
                  (c_t, !env)

                  (* Si ça ne peut pas être une liste on explose *)
                  |t, _ -> (raise (TypesDoNotMatch(string_of_ftype type_list (List_f e1_t), string_of_ftype type_list t)))
                  end

  | _ -> failwith "You should never fall in that case. In Typechecking.infer"


  with x -> error_display node_id x
  ;;
