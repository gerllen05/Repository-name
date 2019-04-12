type tree = Node of int*tree*tree | Leaf;;
let a = Node(1,Node(2,Leaf,Node(3,Leaf,Leaf)),Node(4,Leaf,Leaf));;
let rec glub a = match a with
	Leaf -> 0
	|Node(num,t1,t2) -> 1 + (glub t1) + (glub t2);;
let rec write a m n = match a with
	Leaf -> ()
	|Node(num,t1,t2) -> if m <> n then (write t1 (m+1) n;write t2 (m+1) n) else (print_int num;write t1 (m+1) n;write t2 (m+1) n);;
for i = 0 to (glub a) do (write a 0 i) done;; 

	
