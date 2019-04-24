type tree = Leaf | Node of int*tree*tree;;
let n = read_int();;
let rec bamb k = if k < n then Node(k,Leaf,bamb (k+1)) else Node (k,Leaf,Leaf);;

let rec write a = match a with
	Leaf -> "Leaf"
	|Node(d,t1,t2) -> "Node(" ^ (string_of_int d) ^ "," ^ (write t1) ^ "," ^ (write t2) ^ ")";;
	
print_string (write (bamb 1));;

