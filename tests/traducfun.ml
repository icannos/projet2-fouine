let e1 x= x in
    let nom = 1 in 
    fun s -> ((fun nom -> e1), s)  ;;
