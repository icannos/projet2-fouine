let rec factorielle = fun x ->
  if x = 0 then 1 else x * (factorielle (x-1)) in  prInt (factorielle 10)
