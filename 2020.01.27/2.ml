let n = read_int();;
let rec pet1 g s = match s with
	a::b -> ["(" ^ g ^ "," ^ a ^ ")"] @ (pet1 g b)
	|[] -> [];;
let rec pet2 l s = match l with
	g::f -> (pet1 g s) @ (pet2 f s)
	|[] -> [];;
	
let rec count p n = if p = 0 then pet2 (mul 0 n) (mul (n-1) n) else (pet2 (mul p n) (mul (n-p-1) n)) @ (count (p-1) n)
	and mul k n = if k = 0 then ["*"] else if k = 1 then ["(*,*)"] else count (k-1) k;;
	


List.iter (fun x -> print_string x ; print_string "\n") (mul (n) (n+1));;
