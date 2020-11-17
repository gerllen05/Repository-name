type tree = Leaf| Node of tree*tree*int;;

let t = Node(Node(Leaf,Leaf,0),Node(Node(Leaf,Leaf,1),Leaf,3),2);;

let rec ver t = match t with
	Leaf -> true
	|Node(Leaf,Leaf,e) -> true
	|Node(Node(a,b,c),Leaf,e) -> if c<e then ver (Node(a,b,c)) else false
	|Node(Leaf,Node(r,p,q),e) -> if q>e then ver (Node(r,p,q)) else false
	|Node(Node(a,b,c),Node(r,p,q),e) -> 
		if c<e then 
			(if q>e then 
				((ver (Node(a,b,c))) && (ver (Node(r,p,q))))
			else false)
		else false;;
		
Printf.printf "%b" (ver t);;
	
