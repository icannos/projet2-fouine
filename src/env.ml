
open Display;;

type name = String.t;;


module Environnement = Map.Make(String);;

module VarsSet = Set.Make(String);;

(*création des nos superbes environnements :dire que c'est une string doit suffire, un raffinement possible en définissant le type précis des noms de variables*)


         
 (*cette double définition ne marche pas, or je ne vois pas comment supprimer cette intrication*)
 (*je crois qu'en fait value ne doit être que int, et qu'il faut gérer la cloture autrement *) 
type value = Int of int | Fonction of (value -> value)
and
env = value Environnement.t;; (*ces environnements contiennent des value*)

let print_value = function
  |Int x -> print_int x
  |_ -> raise (Invalid_argument "Cannot print a function")
;;

let print_identifier = function
  |s -> print_string s
;;

let penv_item identifier v =
  print_identifier identifier; print_string " = " ; print_value v ; ps "; ";;

let print_env env = ps "{"; Environnement.iter penv_item env; ps "} \n";;

let safe_add e1 e2 = match e1,e2 with
  |Int x, Int y -> Int(x + y)
  |_ -> raise (Invalid_argument "Cannot sum a function")
;;

let safe_mult e1 e2 = match e1,e2 with
  |Int x, Int y ->Int( x * y)
  |_ -> raise (Invalid_argument "Cannot mult a function")
;;

let safe_sou e1 e2 = match e1,e2 with
  |Int x, Int y -> Int (x - y)
  |_ -> raise (Invalid_argument "Cannot sub a function")
;;

let safe_div e1 e2 = match e1,e2 with
  |Int x, Int y when y <> 0 ->Int( x /  y)
  |_, Int y when y = 0 -> raise (Division_by_zero)
  |_,_ -> raise (Invalid_argument "Cannot sum a function")
;;

let safe_op e1 op e2 = match e1,e2 with
  |Int x, Int y -> op x y
  |_,_ -> raise (Invalid_argument "I can't succeed")
                           
