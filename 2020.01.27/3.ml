let n = read_int();;
let m = read_int();;
let rec pet1 g s = match s with
	a::b -> [max a g] @ (pet1 g b)
	|[] -> [];;
let rec pet2 l s = match l with
	g::f -> (pet1 g s) @ (pet2 f s)
	|[] -> [];;
	
let rec count p n m = if p = 0 then pet2 (mul 0 n m) (mul (n-1) n m) else (pet2 (mul p n m) (mul (n-p-1) n m)) @ (count (p-1) n m)
	and mul k n m = if k = 0 then [m] else if k = 1 then [m + 1] else count (k-1) k (m + 1);;
	
let rec re l = match l with
	a::b -> (if a = m then 1 else 0) + re b
	|[] -> 0;;

print_int (re (mul (n) (n+1) 0));;
