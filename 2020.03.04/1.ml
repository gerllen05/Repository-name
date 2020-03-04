type l = Var of string | App of l * l | Abs of string * l;;
let a = (Abs ("x",App (Var "y", Var "z")));;

let rec prin a = match a with
	Var s -> s 
	|App (y,z) -> "(" ^ (prin y) ^ " " ^ (prin z) ^ ")"
	|Abs (x,y) -> "(\\" ^ x ^ "." ^ (prin y) ^ ")";;
	
print_string (prin a);; 

