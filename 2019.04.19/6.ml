type tree = Leaf | Node of int*tree*tree;;

let n = read_int();;
let rec bamb f = if f < n then Node(f,Leaf,bamb (f+1)) else Node (f,Leaf,Leaf);;

let k = read_int();;

let a = bamb 1;;

let rec check a = match a with
	Leaf -> Leaf
	|Node(d,t1,t2) -> if d <= k then Node(d,check t1,check t2) else Leaf;;

let rec podtree a = match a with
	Leaf -> failwith "No elements < k!!"
	|Node(d,t1,t2) -> if d <= k then check (Node(d,t1,t2)) else (podtree t1);;
	
let rec write a = match a with
	Leaf -> "Leaf"
	|Node(d,t1,t2) -> "Node(" ^ (string_of_int d) ^ "," ^ (write t1) ^ "," ^ (write t2) ^ ")";;
	
print_string (write (podtree (bamb 1)));;
