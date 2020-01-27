let n = read_int();;
let rec count_part n p = if n = 1 then 1 else if p = 0 then 0 else count_part p (p-1) + (count_part n (p-1));;

print_int (count_part n n);; 
	
	
	
