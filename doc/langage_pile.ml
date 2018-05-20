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
  | Acoupler of (instruction list list) (*Pour les couples à gauche d'un let*)
  | Couple of (instruction list list)
