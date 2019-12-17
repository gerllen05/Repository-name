open String;;
let s = "101$";;

let rec tol s n = if n+1 > length s then [] else (match s.[n] with
	'$' -> (-1)::(tol s (n+1))  
	|'1'-> 1::(tol s (n+1))
	|'0'-> 0::(tol s (n+1)) );;
	
List.iter (fun x -> print_int x; print_string " ") (tol s 0);;
