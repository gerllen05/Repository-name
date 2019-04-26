type tree = Braces of tree list ;;
let c = 3;;
let a = Braces[Braces[Braces[];Braces[]];Braces[]];;
let rec write x d = match x with
	Braces (m::n) -> "*" ^ "\n" ^ (String.make ((d+1)*c) ' ') ^ (write m (d+1)) ^ "\n" ^ (write (Braces n) (d+1))
	|Braces [] -> "*";;
	
print_string ("\n" ^ (write a 0) ^ "\n");;

let rec writesk a = match a with
	|Braces (m::n) -> "Braces[" ^ (writesk m) ^ ";" ^ (writesk (Braces n)) ^ "]"
	|Braces [] -> "Braces[]";;

let s = writesk a;;
	
print_string s;;
