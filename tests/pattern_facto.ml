let rec f n =
  match n with
  |0 -> 1
  |x -> x * (f (x-1))
in prInt (f 10)
