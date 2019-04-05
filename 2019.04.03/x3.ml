let n = 5;;
let rec sqr q n = if q < (n-2) then (("*" ^ (String.make (n-2) ' ') ^ "*\n") ^ (sqr (q+1) n)) else ("*" ^ (String.make (n-2) ' ') ^ "*\n");;
print_string ((String.make n '*') ^ "\n" ^ (sqr 1 n) ^ (String.make n '*'));;
