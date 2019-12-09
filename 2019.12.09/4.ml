let l = [1;0;1;1;1;];;
let o = read_int();;

let rec trans l o p = match l with
	[] -> 0
	|a::b -> (a * p) + (trans b o (p * o));;
	
print_int (trans l o 1);;
