type tree = Leaf| Node of tree*tree*int;;

let t = Node(Node(Leaf,Leaf,0),Node(Leaf,Leaf,3),2);;

let rec add t x = match t with
	Leaf -> Node(Leaf,Leaf,x)
	|Node(Leaf,Leaf,e) -> if x<e then Node(Node(Leaf,Leaf,x),Leaf,e) else Node(Leaf,Node(Leaf,Leaf,x),e)
	|Node(a,b,e) -> if x<e then Node(add a x,b,e) else Node(a,add b x,e);;
	
let rec string_of_tree t = match t with
	Leaf -> "*"
	|Node(a,b,c) -> "(" ^ (string_of_tree a) ^ "," ^ (string_of_tree b) ^ "," ^ (string_of_int c) ^ ")";;
	
print_string (string_of_tree (add t 1));;
