let rec f x y z = if x = 0 then (if 3>5 then x else y, if 3>5 then z else y, z+1)
          else f (x-1) y z in f
