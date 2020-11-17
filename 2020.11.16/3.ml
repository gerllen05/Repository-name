type tree = Leaf| Node of tree*tree*int;;

let t = Node(Node(Leaf,Leaf,0),Node(Leaf,Leaf,3),2);;

let rec del t x = match t with
	Leaf -> Leaf
	|Node(Leaf,Leaf,e) -> if x = e then Leaf else Node(Leaf,Leaf,e)
	|Node(Leaf,Node(a,b,c),e) -> if x = e then Node(Leaf,Node(del a c,del b c,c),c) else Node(Leaf,del (Node(a,b,c)) x,e)
	|Node(Node(a,b,c),d,e) -> if x = e then Node(del (Node(del a c,del b c,c)) c,d,c) else Node(del (Node(a,b,c)) x,del d x,e);;
	
let rec string_of_tree t = match t with
	Leaf -> "*"
	|Node(a,b,c) -> "(" ^ (string_of_tree a) ^ "," ^ (string_of_tree b) ^ "," ^ (string_of_int c) ^ ")";;
	
print_string (string_of_tree (del t 2));;
