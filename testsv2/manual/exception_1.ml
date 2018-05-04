let div a b = match b with
|0 -> raise DivisionZero
|x -> a / b
;;

try prInt (div 5 0) with DivisionZero -> prInt 88888
