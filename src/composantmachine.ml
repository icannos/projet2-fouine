exception Notmatched;;

type name = string;;


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
  | Print (*Il y a un print en trop, je ne comprends pas pourquooi*)
  | Ref
  | Aff
  | Bang

type code = instruction list

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
and
 environnement = (name * memslot) list

type pile = memslot list
