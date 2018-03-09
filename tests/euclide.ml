let rec euclide a b = if a = b then a
                      else
                        if a > b then euclide (a-b) b
                        else euclide a (b-a)

    in

    prInt (euclide 360 128)
