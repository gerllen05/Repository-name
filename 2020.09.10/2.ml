let n = read_int();;
let x = (read_line()).[0];;

let rec minor m n = if m*m < n then (m*m)::(minor (m+1) n) else [];;

let rec num m x n = if m < n+1 then 
	(if (string_of_int m).[0] = x then m::(num (m+1) x n) else (num (m+1) x n))
	else [];;
	
Printf.printf "%f" ((float_of_int (List.length (num 1 x n))) /. (float_of_int n));;
