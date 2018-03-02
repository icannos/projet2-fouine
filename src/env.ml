
open Display;;

type name = String.t;;


module Environnement = Map.Make(String);;

module VarsSet = Set.Make(String);;


 
type value = Int of int | Fonction of (value -> value)
and
env = value Environnement.t;; (*ces environnements contiennent des value*)
