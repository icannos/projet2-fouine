let rec ackermann m = fun n ->
                             if m = 0 then (n+1) else
                               if m > 0 then
                                 if n = 0 then (ackermann (m-1) 1)
                                 else if n > 0 then ackermann (m-1) (ackermann m (n-1))
                                 else 0
                               else 0
    in let rec pgcd a b = if a = b then a
                      else
                        if a > b then pgcd (a-b) b
                        else pgcd  a (b-a)
          in


          let f = ref ackermann in prInt (!f 2 4); f := pgcd; prInt (!f 2 4)
