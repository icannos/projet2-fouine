open Composantmachine;;
  
let rec tab n = match n with
  |0-> ""
  |n-> "  " ^ tab (n-1)

let rec affiche_slot slot = match slot with
  |I a -> print_string "affiche_slot "; print_int a;  print_newline ()
  |B a -> print_string (string_of_bool a); print_newline ()
  |Clot (nom, _,_)-> print_string nom; print_newline ()
  |ClotR (nom, _,_, _)-> print_string nom; print_newline ()
  |Lcode code -> print_string "Lcode\n"
  |Lenv env -> print_string "Lenv\n"
  | Eps -> print_string "Epsilon\n"
  | Reference a -> print_string "Reference\n"


let rec affiche_env env = match env with
  | [] -> ()
  | (nom, _)::q -> print_string nom ; affiche_env q

let rec affiche_pile pile = match pile with
  | [] -> ()
  | memslot::q -> affiche_slot memslot; affiche_pile q

let rec joli_code n l s =
  match l with
  | [] -> s
  | Add::q -> joli_code n q (s ^ (tab n) ^ "Add \n")
  | Mul::q -> joli_code n q (s ^ (tab n) ^ "Mul \n")
  | Sub::q -> joli_code n q (s ^ (tab n) ^ "Sub \n")
  | Div::q -> joli_code n q (s ^ (tab n) ^ "Div \n")
  | (C k)::q -> joli_code n q (s ^ (tab n) ^ "C " ^ (string_of_int k) ^ "\n")
  | (Access x)::q -> joli_code n q (s ^ (tab n) ^ "Access "^ x ^ "\n")
  | (Let x)::q -> joli_code n q (s ^ (tab n) ^ "Let " ^x ^"\n")
  | Endlet::q -> joli_code n q (s ^  (tab n) ^"Endlet \n")
  | Ret::q -> joli_code n q (s ^ (tab n) ^ "Ret \n")
  | Apply::q -> joli_code n q (s ^ (tab n) ^ "Apply \n")
  | (Clos (x, inst))::q -> joli_code n q (s ^ (tab n) ^ "Clos(" ^ x ^ (joli_code (n+1)  inst "\n")^ ")\n")
  | IfThenElse(e1,e2)::q -> joli_code n q (s ^ (tab n) ^ "IfThenElse(" ^ (joli_code (n+1) e1 "\n") ^ (joli_code (n+1) e2 ""))
  | Eq::q ->  joli_code n q (s ^ (tab n) ^ "Eq\n")
  | Neq::q ->  joli_code n q (s ^ (tab n) ^ "Neq\n")
  | Lt::q ->  joli_code n q (s ^ (tab n) ^ "Lt\n")
  | Gt::q ->  joli_code n q (s ^ (tab n) ^ "Gt\n")
  | Le::q ->  joli_code n q (s ^ (tab n) ^ "Le\n")
  | Ge::q ->  joli_code n q (s ^ (tab n) ^ "Ge\n")
  | (Rec x)::q ->  joli_code n q (s ^ (tab n) ^ "Rec  " ^x ^"\n")
  | Print::q -> joli_code n q (s ^ (tab n) ^ "Print\n")
  | Ref::q-> joli_code n q (s ^ (tab n) ^ "Ref\n")
  | Aff::q -> joli_code n q (s ^ (tab n) ^"Aff\n")
  | Bang::q -> joli_code n q (s ^ (tab n) ^ "Bang\n")
(*|_ -> "Not yet implemented"*)
  ;;


let affiche_code e = print_string (joli_code 0 e ""); print_newline ();;