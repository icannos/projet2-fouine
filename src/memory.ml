(*fonctions de gestion de la mémoire*)
open Env;;
(*création de notre mémoire : map indéxée par des int*)

module Mem = Map.Make(int);;

type memoire = value Mem.t;; (*définitions analogues aux environnements*)
  
let memoire = Mem.empty;; (*initialisation de la mémoire, ce n'est pas un peu sale d'en faire une ref ?*) (* si d'ailleurs c'en est pas une *)

let add_memory add v  =
  (*fonction qui remplit une adresse*)
  let memoire = Mem.add add v memoire;;

let read_address add =
  (*fonction qui lit une référence*)
  Mem.find add memoire;;
  

let new_address =
  (*en fait je n'arrive pas à voir comment on fait*)
