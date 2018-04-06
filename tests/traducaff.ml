 fun s0 -> let (v1,s1) = e1 s0 in
              let (v2,s2) = e2 s1 in
              let s3 = modify s2 (v1,v2) in
              ((),s3);;
