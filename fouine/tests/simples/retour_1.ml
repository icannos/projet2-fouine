let f g =
  let r = ref 5  in
  let h x =
    let res = g !r in res
   in h
   in
 let g x = x * x in
 let m = f g in

 m 22
