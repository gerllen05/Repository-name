type tree = Leaf| Node of tree*tree*int;;

let t = Node(Node(Leaf,Leaf,0),Node(Leaf,Leaf,3),2);;

let rec del t x lr = match t with
	Leaf -> Leaf
	|Node(a,b,c) -> if x = c then Node(del a x 0,del b x 1,) 
