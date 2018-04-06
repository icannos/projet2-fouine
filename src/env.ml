open Expr;;



module Environnement = Map.Make(String);;

(* Le type unit va correspondre à () et Reference(addr) c'est pour les références *)
type value = Int of int
           |Unit
           |Reference of int
           |Fonction of name * extexpr * env
           |Rec of   name * name *  extexpr * env (*le premier name garde le nom de la fonction*)
           |TSum of name * value list (* Les types sommes *)
           |Cartesian of value list (* les n-uplets *)
           |Listing of value * value (*les listes*)
           |LVide
           |Exn of value (* Constructeur spécial pour gérer les exceptions *)
           |Bool of bool

and

env = value Environnement.t;; (*ces environnements contiennent des value*)
