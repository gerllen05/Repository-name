type tree = Node of int*tree*tree | Leaf;;
let a = Node(2,Node(1,Leaf,Leaf),Node(3,Leaf,Leaf));;
let x = read_int();;
let rec findhard n = match n with
	|Node(d,Leaf,Leaf) -> Leaf
	|Node(d,Node(num,t1,t2),Leaf) -> Node(num,t1,t2)
	|Node(d,Leaf,Node(num,t1,t2)) -> Node(num,t1,t2)
	|Node(d,Node(num1,t1,t2),Node(num2,p1,p2)) -> Node (num2,Node(num1,t1,t2),findhard (Node(num2,p1,p2)));;
let rec find a x = match a with
	Leaf -> failwith "Why are you trying to delete element which isnt in the tree?"
	|Node(num,t1,t2) -> if x = num then (findhard (Node(num,t1,t2))) else (if x < num then find t1 x else find t2 x);;
	
let rec lkp a = match a with
	Leaf -> []
	|Node(num,t1,t2) -> (lkp t1) @ [num] @ (lkp t2);;
let rec upor l = match l with
	[] -> true
	|a::[] -> true
	|a::b::c -> if a > b then false else (upor (b::c));;

Printf.printf "%b" (upor (lkp (find a x)));;

	
