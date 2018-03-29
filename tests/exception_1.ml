let div a b = match b with
|0 -> raise DivisionZero(0)
|x -> a / b
;;

try prInt (div 5 0) with |DivisionZero(0) -> prInt 88888
