type tree = Braces of tree*tree | Leaf ;;
let c = 3;;
let a = Braces(Leaf,Braces(Braces(Leaf,Leaf),Leaf));;
let rec write x d = match x with
	Leaf -> "."
	|Braces(y1,y2) -> "*" ^ "\n" ^ (String.make ((d+1)*c) ' ') ^ (write y1 (d+1)) ^ "\n" ^ (String.make ((d+1)*c) ' ') ^ (write y2 (d+1)) ;;

print_string ("\n" ^ (write a 0) ^ "\n");;

let rec writesk a = match a with
	Leaf -> "Leaf"
	|Braces(y1,y2) -> "Braces(" ^ (writesk y1) ^ "," ^ (writesk y2) ^ ")";;

let s = writesk a;;
	
print_string s;;



