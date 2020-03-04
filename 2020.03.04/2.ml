type l = Var of string | App of l * l | Abs of string * l;;
let a = (Abs ("x",App (Var "y", Var "z")));;

let rec strvar a = match a with
	Var s -> s 
	|App (y,z) -> (strvar y) ^ (strvar z)
	|Abs (x,y) ->  x ^ (strvar y);;

print_string ((strvar a) ^ "$");;
