type tree = Node of (int*int)*tree*tree | Leaf;;
let a = Node((2,222),Node((1,111),Leaf,Leaf),Node((3,333),Leaf,Leaf));;
let x = read_int();;
let rec find a x = match a with
	Leaf -> None
	|Node((c,d),t1,t2) -> if x = c then Some d else (if x < c then find t1 x else find t2 x);;
let print m = match m with
	None -> print_string "None"
	|Some n -> print_int n;;
print (find a x);;
