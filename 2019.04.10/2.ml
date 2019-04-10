type tree = Node of int*tree*tree | Leaf;;
type tree2 = Node2 of int*int 
let a = Node(1,Node(2,Leaf,Node(3,Leaf,Leaf)),Node(4,Leaf,Leaf));;
let rec shir a d = match a with
	Node(num,t1,t2) -> Node2(num,d)::(shir t1 (d+1))::(shir t2 (d+1));;
let rec shiri l d = 
		if d<30 then match l with
	Node2(num,c)::[] -> if c = d then ((string_of_int num) ^ (shiri l (d+1))) else (shiri l (d+1))
	|Node2(num,c)::b -> if c = d then ((string_of_int num) ^ (shiri b d)) else (shiri b d)
	|[] -> [] 
		else "";;
print_string (shiri (shir a 0) 0);;
	
