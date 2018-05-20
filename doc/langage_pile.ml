type instruction =
  | C of int (*Pour les constantes*)
  | Add  | Mul | Sub | Div
  | Let of name (*Fidèle aux notations du cours*)
  | Access of name
  | Endlet
  | Clos of name * (instruction list)
  | Rec of name
  | Ret
  | Apply
  | IfThenElse of (instruction list) * (instruction list)
  | Eq | Neq  | Lt  | Gt  | Le  | Ge
  | Print

  (*Les aspects impératifs*)
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
  | Couple of (instruction list list)

;;
