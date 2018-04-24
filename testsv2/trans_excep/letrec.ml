let a = ref 2 in
    let _ = if 2 < 5 then (prInt !a+5) else (prInt (!a+2))
