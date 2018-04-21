let rec f n = match n with
  |[] -> 0
  |x::q -> prInt x; f q
         
in (f ([3;2;1;5;4]))
