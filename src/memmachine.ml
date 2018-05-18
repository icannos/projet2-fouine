(** Gère la mémoire de la machine à pile*)

open Composantmachine;;


(* Ca c'est pour définir une table de hashage indexée par des int, on donne le test d'égalité et
la fonction de hash, on doit le faire aussi si on utilise des Map indexées par des int mais
en réfléchissant c'est débile de faire une Map dont le cout d'accès est du log n alors que ma table de hashage c'est du temps constant
*)
module IntHash =
        struct
          type t = int
          let equal i j = i=j
          let hash i = i land max_int
        end
;;

module Mem  = Hashtbl.Make(IntHash);;

type memoire_t = memslot Mem.t;;
let memoire = (Mem.create 100);;

Mem.add memoire 0 (I 0);;
Mem.remove memoire 0;;

 (** fonction qui remplit une adresse*)
let add_memory addr v  =  Mem.replace memoire addr v;;


 (** fonction qui lit une référence*)
let read_address addr =  Mem.find memoire addr;;

(** Crée une nouvelle adresse mémoire*)
let new_address ()  = Mem.length memoire;;
