open Expr;;
open Display;;




module Environnement = Map.Make(String);;




 
type value = Int of int
           |Fonction of name * expr * env
           |Rec of   name * name *  expr * env (*le premier name garde le nom de la fonction*)
          (* |Fonction of (value -> value)  Je laisse ça là parce que je trouvais ça élégant *)

and
env = value Environnement.t;; (*ces environnements contiennent des value*)
