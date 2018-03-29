open Env;;





let safe_add e1 e2 = match e1,e2 with
  |Int x, Int y -> Int(x + y)
  |_, Exn x|Exn x, _ -> Exn x
  |_ -> raise (Invalid_argument "Cannot sum a function")
;;

let safe_mult e1 e2 = match e1,e2 with
  |Int x, Int y ->Int( x * y)
  |_, Exn x|Exn x, _ -> Exn x
  |_ -> raise (Invalid_argument "Cannot mult a function")
;;

let safe_sou e1 e2 = match e1,e2 with
  |Int x, Int y -> Int (x - y)
  |_, Exn x|Exn x, _ -> Exn x
  |_ -> raise (Invalid_argument "Cannot sub a function")
;;

let safe_div e1 e2 = match e1,e2 with
  |Int x, Int y when y <> 0 ->Int( x /  y)
  |_, Int y when y = 0 -> raise (Division_by_zero)
  |_, Exn x|Exn x, _ -> Exn x
  |_,_ -> raise (Invalid_argument "Cannot sum a function")
;;

let safe_op e1 op e2 = match e1,e2 with
  |Int x, Int y -> Bool (op x y)
  |_, Exn x|Exn x, _ -> Exn x
  |_,_ -> raise (Invalid_argument "I can't succeed")
