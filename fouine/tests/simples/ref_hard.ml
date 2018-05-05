let a = ref 10 in
    let b = ref a in
    prInt (! !b)
