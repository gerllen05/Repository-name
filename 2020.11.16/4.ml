type tree = Leaf| Node of tree*tree*int;;

let t = Node(Node(Leaf,Leaf,1),Node(Leaf,Leaf,3),2);;

let rec string_of_tree t = match t with
	Leaf -> "*"
	|Node(a,b,c) -> "(" ^ (string_of_tree a) ^ "," ^ (string_of_tree b) ^ "," ^ (string_of_int c) ^ ")";;
	
print_string (string_of_tree t);;
