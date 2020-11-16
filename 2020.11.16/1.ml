type tree = Leaf| Node of tree*tree*int;;

let t = Node(Node(Leaf,Leaf,1),Node(Leaf,Leaf,3),2);;

let rec mem t x = match t with
	Leaf -> false
	|Node(a,b,c) -> if c = x then true else (if x < c then mem a x else mem b x);;
	

Printf.printf "%b" (mem t 4);;


