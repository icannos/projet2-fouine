let rec ackermann = fun m -> fun n ->
                             if m = 0 then (n+1) else
                               if m > 0 then
                                 if n = 0 then (ackermann (m-1) 1)
                                 else if n > 0 then ackermann (m-1) (ackermann m (n-1))
                                 else 0
                               else 0
    in prInt (ackermann 3 4)
