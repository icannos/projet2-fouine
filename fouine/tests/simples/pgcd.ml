let rec pgcd a b = if a = b then a
                      else
                        if a > b then pgcd (a-b) b
                        else pgcd  a (b-a)

    in

    prInt (pgcd 360 128)
