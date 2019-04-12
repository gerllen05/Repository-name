type tree = Node of int*tree*tree | Leaf;;
let a = Node(2,Node(1,Leaf,Leaf),Node(3,Leaf,Leaf));;
let x = read_int();;
let rec fill a x = match a with
	Leaf -> Node(x,Leaf,Leaf)
	|Node(num,t1,t2) -> if x < num then Node(num,(fill t1 x),t2) else Node(num,t1,(fill t2 x));;
let rec lkp a = match a with
	Leaf -> []
	|Node(num,t1,t2) -> (lkp t1) @ [num] @ (lkp t2);;
let rec upor l = match l with
	[] -> true
	|a::[] -> true
	|a::b::c -> if a > b then false else (upor (b::c));;
Printf.printf "%b"(upor (lkp (fill a x)));;
	
