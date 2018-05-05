open Composantmachine;;
  
let rec tab n = match n with
  |0-> ""
  |n-> "  " ^ tab (n-1)

let rec affiche_slot slot = match slot with
  |I a -> print_string "affiche_slot"; print_int a
  |B a -> print_string (string_of_bool a)
  |Clot (nom, _,_)-> print_string nom
  |ClotR (nom, _,_, _)-> print_string nom
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
  | Add::q -> joli_code n q (s ^ "Add \n")
  | Mul::q -> joli_code n q (s ^ "Mul \n")
  | Sub::q -> joli_code n q (s ^ "Sub \n")
  | Div::q -> joli_code n q (s ^ "Div \n")
  | (C k)::q -> joli_code n q (s ^ "C " ^ (string_of_int k) ^ "\n")
  | (Access x)::q -> joli_code n q (s ^ "Access "^ x ^ "\n")
  | (Let x)::q -> joli_code n q (s ^ "Let " ^x ^"\n")
  | Endlet::q -> joli_code n q (s ^ "Endlet \n")
  | Ret::q -> joli_code n q (s ^ "Ret \n")
  | Apply::q -> joli_code n q (s ^ "Apply \n")
  | (Clos (x, inst))::q -> joli_code n q (s ^ "Clos(" ^ x ^ (joli_code (n+1)  inst "\n")^ ")\n")
  | IfThenElse(e1,e2)::q -> joli_code n q (s ^ "IfThenElse(" ^ (joli_code (n+1) e1 "\n") ^ (joli_code (n+1) e2 ""))
  | Eq::q ->  joli_code n q (s ^ "Eq\n")
  | Neq::q ->  joli_code n q (s ^ "Neq\n")
  | Lt::q ->  joli_code n q (s ^ "Lt\n")
  | Gt::q ->  joli_code n q (s ^ "Gt\n")
  | Le::q ->  joli_code n q (s ^ "Le\n")
  | Ge::q ->  joli_code n q (s ^ "Ge\n")
  | (Rec x)::q ->  joli_code n q (s ^ "Rec  " ^x ^"\n")
  | Print::q -> joli_code n q (s ^ "Print\n")
  | Ref::q-> joli_code n q (s ^ "Ref\n")
  | Aff::q -> joli_code n q (s ^"Aff\n")
  | Bang::q -> joli_code n q (s ^ "Bang\n")
(*|_ -> "Not yet implemented"*)
  ;;


let affiche_code e = print_string (joli_code 0 e ""); print_newline ();;
