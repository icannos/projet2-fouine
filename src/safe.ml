(** Opérations sécurisées pour éviter les explosions de fouine*)
open Env;;
open Affichage;;


let safe_add e1 e2 = match e1,e2 with
  |Int x, Int y -> Int(x + y)
  |_, Exn x|Exn x, _ -> Exn x
  |_ -> raise (Invalid_argument ("Cannot sum: "^ (string_of_value e1) ^" and " ^ (string_of_value e2)))
;;

let safe_mult e1 e2 = match e1,e2 with
  |Int x, Int y ->Int( x * y)
  |_, Exn x|Exn x, _ -> Exn x
  |_ -> raise (Invalid_argument ("Cannot mult: "^ (string_of_value e1) ^" and " ^ (string_of_value e2)))
;;

let safe_sou e1 e2 = match e1,e2 with
  |Int x, Int y -> Int (x - y)
  |_, Exn x|Exn x, _ -> Exn x
  |_ -> raise (Invalid_argument ("Cannot sou: "^ (string_of_value e1) ^" and " ^ (string_of_value e2)))
;;

let safe_div e1 e2 = match e1,e2 with
  |Int x, Int y when y <> 0 ->Int( x /  y)
  |_, Int y when y = 0 -> raise (Division_by_zero)
  |_, Exn x|Exn x, _ -> Exn x
  |_,_ -> raise (Invalid_argument ("Cannot div: "^ (string_of_value e1) ^" and " ^ (string_of_value e2)))
;;

let safe_op e1 op e2 = match e1,e2 with
  |Int x, Int y -> Bool (op x y)
  |_, Exn x|Exn x, _ -> Exn x
  |_,_ -> raise (Invalid_argument ("Cannot compare: "^ (string_of_value e1) ^" and " ^ (string_of_value e2)))
