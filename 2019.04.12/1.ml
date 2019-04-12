type tree = Node of int*tree*tree | Leaf;;
let a = Node(2,Node(1,Leaf,Leaf),Node(3,Leaf,Leaf));;
let rec lkp a = match a with
	Leaf -> []
	|Node(num,t1,t2) -> (lkp t1) @ [num] @ (lkp t2);;
let rec upor l = match l with
	[] -> "true"
	|a::[] -> "True"
	|a::b::c -> if a > b then "False" else (upor (b::c));;
print_string (upor (lkp a));;
