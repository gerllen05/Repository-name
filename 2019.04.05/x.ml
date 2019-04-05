type tree = Braces of tree | Star;;
let a = Braces (Braces (Braces (Braces Star)))
let rec str a = match a with
	Braces b -> "(" ^ (str b) ^ ")"
	|Star -> "*" ;;
print_string (str a);;
