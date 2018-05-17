fun x y z t u -> (if 3 > 5 then x else z, if 3 > 5 then y else z,
  if 3 > 5 then z else t,
  if 3 > 5 then t else u,
  u+1 )
