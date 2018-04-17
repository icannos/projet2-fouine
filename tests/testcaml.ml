


(( fun s1 -> let (a,s2) = (( fun s3 -> let (v1,s4) = (( fun s6 -> (5,s6) ) s3)
 in  let (v2,s5) = (( fun s7 -> (3,s7) ) s4)
 in  (if (( fun s6 -> (5,s6) ) > ( fun s7 -> (3,s7) ))
 then
 ( ( fun s8 -> (2,s8) ))
 else
 (( fun s9 -> (3,s9) )),s5) ) s1)
 in  (( fun s10 -> (0,s10) ) s2) ) [])
