type tree = Node of int*tree*tree | Leaf;;
let a = Node(2,Node(1,Leaf,Leaf),Node(3,Leaf,Leaf));;
let x = read_int();;
let rec find a x = match a with
	Leaf -> "False"
	|Node(num,t1,t2) -> if x = num then "True" else (if x < num then find t1 x else find t2 x);;
print_string (find a x);;
