((( fun k1 -> ( fun kE1 -> ((( fun k2 -> ( fun kE2 -> ((( fun k3 ->
  ( fun kE3 -> (k3 5) ) ) ( fun va3 -> (k2  (ref va3)) )) kE2) ) ) 
  ( fun va4 -> (k1  !va4) )) kE1) ) ) ( fun va1 -> va1 )) ( fun va2 -> va2 ))
