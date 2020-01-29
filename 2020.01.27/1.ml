let n = read_int();;

let rec count p n = if p = 0 then (mul 0 n) * (mul (n-1) n) else (mul p n) * (mul (n-p-1) n) + (count (p-1) n)
	and mul k n = if k = 0 then 1 else if k = 1 then 1 else count (k-1) k;;

print_int (count (n-2) (n-1));;
