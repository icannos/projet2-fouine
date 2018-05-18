open Composantmachine;;


(*Le cas <0 est malheureusement présent dans des cas vicieux de n uplets qui déclenchent des endlet peu contrôlés*)
let rec tab n = if n <= 0 then "" else "  " ^ tab (n-1)

let rec join sep liste = match liste with
  | [] -> ""
  | [a] -> a
  | a::q -> a ^ sep ^ join sep q;;

let affiche_uplet a= print_string "Uplet (";
 let rec aux s liste = match liste with
   |[] -> print_string (s ^ ")\n")
   |[I a] -> aux (s ^(string_of_int a)) []
   |[x] -> aux (s ^ "NAN") []
   |(I a)::q -> aux (s ^(string_of_int a)^",") q
   | _::q -> aux (s ^ "NAN , ") q
in aux "" a;;
                                                     

let rec affiche_slot slot = match slot with
  |I a -> print_string "affiche_slot "; print_int a;  print_newline ()
  |B a -> print_string (string_of_bool a); print_newline ()
  |Clot (nom, _,_)-> print_string nom; print_newline ()
  |ClotR (nom, _,_, _)-> print_string nom; print_newline ()
  |Lcode code -> print_string "Lcode\n"
  |Lenv env -> print_string "Lenv\n"
  | Eps -> print_string "Epsilon\n"
  | Reference a -> print_string "Reference\n"
  | Exception -> print_string "Exception\n"
  | Ignore -> print_string "Ignore\n"
  | Uplet a -> affiche_uplet a
  | Valcouple a -> print_string "Valcouple\n"
  | Amatcher a -> print_string "Amatcher\n"


let rec affiche_env env = match env with
  | [] -> ()
  | (nom, _)::q -> print_string nom; print_newline() ; affiche_env q

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
  | (Let x)::q -> joli_code (n+1) q (s ^ (tab n) ^ "Let " ^x ^"\n")
  | Endlet::q -> joli_code (n-1) q (s ^  (tab (n-1)) ^"Endlet \n")
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
  | Raise::q -> joli_code n q (s ^ (tab n) ^ "Raise\n")
  | Beginwith::q -> joli_code (n+1) q (s ^ (tab n) ^ "Beginwith\n")
  | Endwith::q -> joli_code n q (s ^ (tab (n-1)) ^ "Endwith\n")
  | Endexcep::q -> joli_code (n-1) q (s ^ (tab (n-1)) ^ "Endexcep\n")
  | Couple(liste)::q -> joli_code n q (s ^ (tab n) ^"Couple (\n" ^ (joli_couple (n+1) liste "") ^ ")\n")
  | Acoupler(liste)::q -> joli_code n q (s ^ "Couple (\n" ^ (joli_couple (n+1) liste "") ^ ")\n")
  | Ajoutcouple::q -> joli_code n q (s ^(tab n) ^ "Ajoutcouple\n")
(*|_ -> "Not yet implemented"*)
and joli_couple n liste s = match liste with
  | [] -> s
  | [t] ->joli_couple n [] (s ^ (joli_code n t ""))
  | t::q -> joli_couple n q (s ^ (joli_code n t "") ^ ",")
             
                                           
  ;;


let affiche_code e = print_string (joli_code 0 e ""); print_newline ();;
