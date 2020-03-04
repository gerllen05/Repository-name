type l = Var of string | App of l * l | Abs of string * l;;
let a = (Abs ("x",App (Var "y", Var "z")));;
let b = "y"
let c = "x";;

let rec change a b c = match a with
	Var s -> if s = b then Var c else Var s
	|App (y,z) -> App (change y b c,change z b c)
	|Abs (x,y) -> if x = b then Abs (x,y) else Abs (x,change y b c);;
	
let rec prin a = match a with
	Var s -> s 
	|App (y,z) -> "(" ^ (prin y) ^ " " ^ (prin z) ^ ")"
	|Abs (x,y) -> "(\\" ^ x ^ "." ^ (prin y) ^ ")";;
	
print_string (prin (change a b c));;
