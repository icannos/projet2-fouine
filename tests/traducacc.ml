let e x =x in
    let read = () in
    fun s -> let (l,s1) = e s in
             let v = read l s1 in (v,s1);;
