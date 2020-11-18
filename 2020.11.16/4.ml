type tree = Leaf| Node of tree*tree*int;;

let t = Node(Node(Leaf,Leaf,1),Node(Leaf,Node(Leaf,Leaf,4),3),2);;

let rec check p x = match p with 
	a::b -> if x = a then true else check b x
	|[] ->  false;;
	
let rec sticks p x n = if n<x then  (if (check p n) then "| " ^ (sticks p x (n+2)) else "  " ^ (sticks p x (n+2))) else "";;

let rec string_of_tree l x p = match l with
	Leaf -> print_string "*"
	|Node(a,b,c) -> print_string ((string_of_int c) ^ "\n" ^ (sticks p x 0) ^ "+-");
		if a = Leaf then print_string "*" else string_of_tree a (x+2) (x::p);
	print_string ("\n" ^ (sticks p x 0) ^ "\\-") ; 
		if b = Leaf then print_string "*" else string_of_tree b (x+2) p;;

string_of_tree t 0 [];;
