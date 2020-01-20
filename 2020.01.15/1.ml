let n = read_int();;

exception Found of int list;;

let rec check a b c l = match l with
	[] -> true
	|hd::tl -> if hd = a - 1 || hd = b || hd = c + 1 then false else check (a - 1) b (c + 1) tl;;
	
let rec queen col placed = 
	if col > n-1 then raise (Found placed) else 
		for i = 0 to n - 1 do if check i i i placed then queen (col + 1) (i::placed) done;;

let a = Array.make_matrix n n ". ";;	

let rec neo l k = match l with
	[] -> a
	|b::c ->  a.(k).(b) <- "$ "; neo c (k-1) ;;

try queen 0 [] with Found placed -> Array.iter (fun x -> Array.iter (fun y -> Printf.printf "%s" y) x; print_string "\n") (neo placed (n-1));;


		
	
	

