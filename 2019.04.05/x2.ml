type tree = Braces of tree | Star;;
let c = 3;;
let a = Braces (Braces (Braces (Braces Star)))(Braces (Braces Star);;
let rec str a d= match a with
	Braces b -> (String.make (d*c) ' ') ^ "(\n" ^ (str b (d+1)) ^ (String.make (d*c) ' ') ^ ")\n"
	|Star -> (String.make (d*c) ' ') ^ "*\n" ;;
print_string (str a 0);;
