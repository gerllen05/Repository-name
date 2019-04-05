type comma = Braces of comma*comma | Star;;
let c = 3;;
let a = Braces (Braces (Braces (Star,Star) , (Braces (Star,Star))) , (Braces (Star,Star)));;
let rec str a d= match a with
	Braces (b1,b2) -> (String.make (d*c) ' ') ^ "(\n" ^ (str b1 (d+1)) ^ (str b2 (d+1)) ^ (String.make (d*c) ' ') ^ ")\n"
	|Star -> (String.make (d*c) ' ') ^ "*\n" ;;
print_string (str a 0);;
