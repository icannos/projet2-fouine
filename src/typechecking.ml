open Expr
open Env
open Errmgr
open Display

let free_type_counter = ref (-1);;

(*Renvoie la lettre associée à un entier entre 0 et 25*)
let intToLetter c =String.make 1 (char_of_int (c + 97));;

(*Renvoie le polymorphisme voulu pour un entier donne ie 0 renvoie a' *)
let int_to_letters n = let num = n/26 in
                       if num = 0 then  ((intToLetter n) ^ "'")
                       else ((intToLetter (n mod 26)) ^ (string_of_int num) ^ "'");;
  

let new_free_type () =
free_type_counter:= 1+ !free_type_counter;
("'" ^ (string_of_int !free_type_counter))
;;

module EnvType = Map.Make(String);;


type f_type = Int_f
| List_f of f_type (* On stocke le type d'élément stockés dans la liste *)
| Fun_f of f_type * f_type (* Une fonction fouine, de type truc donne bidule *)
| Cart_f of f_type list (* Un produit cart c'est une liste de type*)
| UserType of string (* Nom du type somme défini par le programmeur *)
| TypeOf of string (* Pour stocker des trucs par encore typés *)
| Ref_f of f_type (* Si on est une ref on connait le type du truc pointé*)
| Unit_f

and
(* Pour stocker le type des variables *)
env_type_t = f_type EnvType.t
;;

(* Convertit un objet type en une chaîne lisible *)
let rec string_of_ftype type_list = function
|Int_f -> "int"
|Fun_f(t1,t2) -> "(" ^ (string_of_ftype type_list t1) ^ " -> " ^ (string_of_ftype type_list t2) ^ ")"
|List_f t -> (string_of_ftype type_list t) ^ " list"
|Cart_f l -> "(" ^ (join " * " (List.map (string_of_ftype type_list) l)) ^ ")"
|UserType s -> s ^ " (aka "^ string_of_ftype type_list (EnvType.find s type_list) ^ " )"
|TypeOf s -> s
|Ref_f t -> "ref " ^ string_of_ftype type_list t
|Unit_f -> "unit"
;;

let print_envtype type_list k t = ps (k ^ ": " ^ (string_of_ftype type_list t) ^" ");;

(* On stocke les types définis par l'utilisateur *)
type type_list_t = f_type EnvType.t;;

(* Utilitaires pour lire les types dans l'environnement*)
let updatevar env x v = env := EnvType.add x v !env;;
let getvartype env x = try EnvType.find x !env
        with _ -> env := (EnvType.add x (TypeOf(x)) (!env)); TypeOf x
;;

(* Le find de l'union find *)
let rec parent_t env x = match getvartype env x with
  | TypeOf a when a = x -> TypeOf x
  | TypeOf a -> parent_t env a
  | x -> x
;;



(* Récupère les types des variables pour les sauvergarder lorsqu'on les écrase*)
let save_vars patt env =
    let ids_saved = ref [] in

    let save_and_reset x = if EnvType.mem x !env then
    ids_saved := (x, EnvType.find x !env)::!ids_saved; env := EnvType.remove x !env
    in
    VarsSet.iter save_and_reset (getIdentifiersInConstr patt); !ids_saved

;;
(* On remet les variables sauvegardées*)
let rec setback_vars env = function
|[] -> ()
|(x,t)::q -> env := EnvType.add x t !env; setback_vars env q
;;

let rec type_arg env = function
|Int_f -> Int_f
|Fun_f(t1,t2) -> Fun_f(type_arg env t1, type_arg env t2)
|List_f t ->List_f (type_arg env t)
|Cart_f l -> Cart_f (List.map (type_arg env) l)
|UserType s -> UserType s
|TypeOf s when EnvType.mem s !env -> getvartype env s
|Ref_f t -> Ref_f (type_arg env t)
|Unit_f -> Unit_f
|_ -> failwith "Something gone wrong with Typechecking.type_arg"
;;


(* Décide si deux trucs peuvent avoir même type, et si oui renvoie le type
convenant aux deux trucs, sinon explose*)
let t_unify (e_t1 : f_type) (e_t2 : f_type) (env : env_type_t ref) (type_list : type_list_t) =
  let rec unif e_t1 e_t2 =
    match e_t1, e_t2 with
    |x, y when x = y -> x
    |TypeOf a, TypeOf b -> updatevar env a (TypeOf a);updatevar env b (TypeOf a); TypeOf a
    |TypeOf a, x |x, TypeOf a -> updatevar env a x;x
    |Fun_f(x1, y1), Fun_f(x2, y2) -> Fun_f(unif x1 x2 , unif y1 y2)
    |Ref_f(x1), Ref_f(x2) -> Ref_f(unif x1 x2)
    |List_f(x1), List_f(x2) -> List_f (unif x1 x2)
    |UserType s, x |x, UserType s -> unif (EnvType.find s type_list) x
    |Cart_f l1, Cart_f l2 when (List.length l1 = List.length l2) ->
    Cart_f (List.map2 unif l1 l2)

    | _ -> raise (TypesDoNotMatch (string_of_ftype type_list e_t1, string_of_ftype type_list e_t2))
  in unif e_t1 e_t2
;;


let rec infer ee (env : env_type_t) (type_list : type_list_t) =
  let (node_id, e) = ee in (* Le node_id est important ici pour râler concernant
    les types qui matchent pas *)

    try
  match e with
  | Uni -> Unit_f, env
  | Const x -> Int_f, env
  | Identifier (x,_) -> let env = ref env in parent_t env x, !env
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
                  let (e_t, envir) = infer e env type_list in
                  let env = ref envir in
                  (* On essaie de matcher les 2*)
                  let _ = t_unify (Fun_f(e_t, expr_t)) (Fun_f(patt_t, expr_t)) env type_list in
                  (type_arg env expr_t , !env) (* On renvoie le type de retour de la fonction*)

                  |TypeOf a, envir -> let (e_t, envir) = infer e env type_list in
                            let env = ref envir in
                            let return_type = TypeOf (new_free_type ()) in
                            let _ = t_unify (TypeOf a) (Fun_f(e_t, return_type)) env type_list in
                            (return_type, !env)


                  (* Si on connait pas on explose*)
                |x, _-> raise (NotFunction (string_of_ftype type_list x))
                end

  |Let((patt, e1), e2) ->
  let env = ref env in
  let svars = save_vars patt env in
  let (e1_t, envir) = infer e1 !env type_list in

  let (patt_t, envir) = infer patt envir type_list in
  let env = ref envir in let _ = t_unify e1_t patt_t env type_list   in
  setback_vars env svars;
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
        let _ = t_unify t e2_t env type_list in (Unit_f, !env)
        |x -> raise (TypesDoNotMatch
          (string_of_ftype type_list x, string_of_ftype type_list (Ref_f e2_t) ))
        end

  |PrintInt e -> let (e_t, envir) = infer e env type_list in
  let env = ref envir in let _ = t_unify e_t Int_f env type_list in
    (Int_f, !env)

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

  |Vide -> (List_f(TypeOf(new_free_type ())), env)
  |Liste(e1, e2) -> let (e1_t, envir) = infer e1 env type_list in
                  begin match infer e2 envir type_list with
                  |List_f(l_t), envir -> let env = ref envir in
                    t_unify (List_f e1_t) (List_f l_t) env type_list, !env
                  |TypeOf x, envir -> let env = ref envir in t_unify (List_f e1_t) (TypeOf x) env type_list, !env
                  |t, _ -> (raise (TypesDoNotMatch(string_of_ftype type_list (List_f e1_t), string_of_ftype type_list t)))
                  end

  | _ -> failwith "You should never fall in that case. In Typechecking.infer"


  with x -> error_display node_id x
  ;;
