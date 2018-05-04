let newstack u = (0, ref ()) in
 let isempty s =
  let (l, e) = s
  in if l = 0 then 1 else 0
 in
 let pop s =
  let (l, e) = s in
  let (x, en) = !e
  in (x, (l - 1, en))
 in
 let push x s =
  let (l , e) = s
  in (l + 1, ref (x, e))
in
 let gettop s =
  let (l , e) = s in
  let (t , sn) = !e
  in t
 in

 let s = push 2 (push 20 (newstack 5)) in

 let (a, sn) = pop s in

 let b = gettop sn in

 let (c, snn) = pop sn in

 let empty = isempty snn in

 if empty = 1 then prInt (a + b + c) else prInt 0
