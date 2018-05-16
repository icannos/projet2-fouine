let a =3 in
    let b = 4 in
    let (a, b) = (let c = 3 in let d = (b, prInt a) in (d, d),prInt b) in
    (prInt b)
