let n = read_int();;
let x = (read_line()).[0];;

let rec minor m n = if m*m < n then (m*m)::(minor (m+1) n) else [];;

let rec sqr m x n = if m*m < n then 
	(if (string_of_int (m*m)).[0] = x then (m*m)::(sqr (m+1) x n) else (sqr (m+1) x n))
	else [];;
	
Printf.printf "%f" ((float_of_int (List.length (sqr 1 x n))) /. (float_of_int (List.length (minor 1 n))));;
