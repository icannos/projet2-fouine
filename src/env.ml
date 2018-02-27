
open Display;;

type name = String.t;;


module Environnement = Map.Make(String);;

(*création des nos superbes environnements :dire que c'est une string doit suffire, un raffinement possible en définissant le type précis des noms de variables*)


         
 (*cette double définition ne marche pas, or je ne vois pas comment supprimer cette intrication*)
 (*je crois qu'en fait value ne doit être que int, et qu'il faut gérer la cloture autrement *) 
type value = int
       (*  | Cloture of fun * env *) (*J'ai commenté ce morceau car en vrai je ne sais pas si le type fun est approprié ce sera une question à poser vendredi *)
(*qui sont soit entières soit des fonctions, définies avec leur vieil environnement*)
;;
type env = value Environnement.t;; (*ces environnements contiennent des value*)

let print_value = function
  |x -> print_int x
;;

let print_identifier = function
  |s -> print_string s
;;

let penv_item identifier v =
  print_identifier identifier; print_string " = " ; print_value v ; ps "; ";;

let print_env env = ps "{"; Environnement.iter penv_item env; ps "} \n";;

  
