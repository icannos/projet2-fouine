open Expr
open Display;;

type name = String.t;;


module Environnement = Map.Make(String);;

module VarsSet = Set.Make(String);;


 
type value = Int of int
           |Fonct of name * expr * env
           |Fonction of (value -> value) (* Je laisse ça là parce que je trouvais ça élégant *)

(*à changer il faut un truc de forme x * expr * env*)
and
env = value Environnement.t;; (*ces environnements contiennent des value*)
