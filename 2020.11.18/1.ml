type ('k,'v) tree = Leaf| Node of ('k,'v) tree * ('k,'v) tree * ('k*'v);;

let t = Node(Node(Leaf,Leaf,(1,"Melon")),Node(Leaf,Leaf,(3,"Pumpkin")),(2,"Carrot"));;

let rec mem t k = match t with
	Leaf -> false
	|Node(a,b,(c,d)) -> if c = k then true else (if k < c then mem a k else mem b k);;

let rec find t k v = match t with
	Leaf -> false
	|Node(a,b,(c,d)) -> if c = k then (if v = d then true else false) else (if k < c then find a k v else find b k v);;
	
let rec add t k v =  match t with
	Leaf -> Node(Leaf,Leaf,(k,v))
	|Node(a,b,(c,d)) -> if c = k then Node(a,b,(k,v)) else (if c < k then Node(add a k v,b,(c,d)) else Node(a,add b k v,(c,d)));;
	
let rec del t k = match t with 
	Leaf -> Leaf
	|Node(Leaf,Leaf,(e,f)) -> if e = k then Leaf else Node(Leaf,Leaf,(e,f))
	|Node(Leaf,Node(a,b,(c,d)),(e,f)) -> if e = k then Node(Leaf,del (Node(a,b,(c,d))) c,(c,d)) else Node(Leaf,del (Node(a,b,(c,d))) k,(e,f)) 
	|Node(Node(a,b,(c,d)),r,(e,f)) -> if e = k then Node(del (Node(a,b,(c,d))) c,r,(c,d)) else Node(del (Node(a,b,(c,d))) e,del r k,(e,f));;
	


let rec check p x = match p with 
	a::b -> if x = a then true else check b x
	|[] ->  false;;
	
let rec sticks p x n = if n<x then  (if (check p n) then "|   " ^ (sticks p x (n+4)) else "    " ^ (sticks p x (n+4))) else "";;

let rec string_of_tree l x p = match l with
	Leaf -> print_string "*"
	|Node(a,b,(c,d)) -> print_string ("(" ^ (string_of_int c) ^ "," ^ d ^ ")" ^ "\n" ^ (sticks p x 0) ^ "+-");
		if a = Leaf then print_string "*" else string_of_tree a (x+4) (x::p);
	print_string ("\n" ^ (sticks p x 0) ^ "\\-") ; 
		if b = Leaf then print_string "*" else string_of_tree b (x+4) p;;

string_of_tree (add t 4 "Potato") 0 [];;
string_of_tree (del t 3) 0 [];;
Printf.printf "\n%b" (mem t 4);;
Printf.printf "\n%b" (find t 1 "Melon");;

