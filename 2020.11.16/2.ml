type tree = Leaf| Node of tree*tree*int;;

let t = Node(Node(Leaf,Leaf,0),Node(Leaf,Leaf,3),2);;

let rec add t x lr = match t with
	Leaf -> Node(Leaf,Leaf,x)
	|Node(a,b,c) -> if lr = 0 then (if x<c then Node(add a x 0,b,c) else Node(add a c 0,b,x)) else (if x<c then Node(a,add b x 1,c) else Node(a,add b c 1,x));;
	

let rec string_of_tree t = match t with
	Leaf -> "*"
	|Node(a,b,c) -> "(" ^ (string_of_tree a) ^ "," ^ (string_of_tree b) ^ "," ^ (string_of_int c) ^ ")";;
	
print_string (string_of_tree (add t 1 0));;
