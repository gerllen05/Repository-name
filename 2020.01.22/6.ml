let n = read_int();;
let a = n;;
let rec count_parts n p l k = 
	if n = k 
	then (List.iter (fun x -> print_int x;print_string " ") (if (List.length (n::l)) mod 2 = 0 then (n::l) else []); if (List.length l) mod 2 = 1 then print_string "\n" else print_string "") 
	else 
		if p = 0 
		then () 
		else (count_parts p (p-1) ((n-p)::l) k;(count_parts n (p-1) l k));;

let rec parts n p l k = if k = a then count_parts n p l k else (count_parts n p l k;parts n p l (k+1));;

parts n (n-1) [] 0;;
