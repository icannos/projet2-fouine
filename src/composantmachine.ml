(** Ce module définit les composantes nécessaires à l'implémentation de la machine à pile*)

exception Notmatched;;

type name = string;;

(** Notre langage de la machine à pile*)
type instruction =
  | C of int
  | Add
  | Mul
  | Sub
  | Div
  | Let of name
  | Access of name
  | Endlet
  | Clos of name * (instruction list)
  | Rec of name
  | Ret
  | Apply
  | IfThenElse of (instruction list) * (instruction list)
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge
  | Print

  (*Les aspects récursifs*)
  | Ref
  | Aff
  | Bang

(*Les exceptions*)
  | Raise
  | Beginwith
  | Endwith
  | Endexcep

  (*Les couples*)
  | Ajoutcouple
  | Acoupler of (instruction list list)
  | Couple of (instruction list list)




(** Un programme pour la machine à pile est une liste d'instructions*)
type code = instruction list

(** Représente une case mémoire de notre machine à pile*)
type memslot = I of int
             | B of bool
             (*Pour les fonctions*)
             |Clot of (name * instruction list * environnement)
             |ClotR of (name * name * instruction list * environnement)
             |Lcode of instruction list
             |Lenv of environnement
             | Eps
             (*Pour les aspects impératifs*)
             | Reference of int

             (*Pour les exceptions*)
             | Exception
             | Ignore (*Je pense qu'un seul suffit mais c'est plus clair pour l'instant*)

            (*Pour les couples*)
             | Uplet of (memslot list)
             | Amatcher of (memslot list)
             | Valcouple of memslot

and
(** L'environnement est un ensemble de cases mémoire*)
 environnement = (name * memslot) list
(** La pile est un ensemble de cases mémoire*)
type pile = memslot list
