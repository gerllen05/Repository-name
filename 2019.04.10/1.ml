type tree = Node of int*tree*tree | Leaf;;
let a = Node(1,Node(2,Leaf,Node(3,Leaf,Leaf)),Node(4,Leaf,Leaf));;
let rec klp a = match a with
	Leaf -> ""
	|Node(num,t1,t2) -> (string_of_int num) ^ (klp t1) ^ (klp t2);;
let rec lkp a = match a with
	Leaf -> ""
	|Node(num,t1,t2) -> (klp t1) ^ (string_of_int num) ^ (klp t2);;
let rec lpk a = match a with
	Leaf -> ""
	|Node(num,t1,t2) -> (klp t1) ^ (klp t2) ^ (string_of_int num);;
print_string ((klp a) ^ " ");;
print_string ((lkp a) ^ " ");;
print_string (lpk a);; 
