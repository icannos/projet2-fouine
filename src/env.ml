(** Définit les type nécessaires à l'intrepeter fouine et au noyau de fouine*)

open Expr;;



module Environnement = Map.Make(String);;

(** Définit le type des valeurs internes à l'interpreter *)
type value = Int of int
           |Unit
           |Reference of int
           |Fonction of extexpr * extexpr * env
           |Rec of   name * extexpr *  extexpr * env (** le premier name garde le nom de la fonction*)
           |TSum of name * value list (** Les types sommes *)
           |Cartesian of value list (** les n-uplets *)
           |Listing of value * value (** les listes*)
           |LVide
           |Exn of value (** Constructeur spécial pour gérer les exceptions *)
           |Bool of bool

and

env = value Environnement.t;; (** ces environnements contiennent des value*)
