type l = Var of string * int | App of l * l | Abs of string * l;;
let a = App (Var ("x",2),Abs ("z",Var ("z",9)));;

let rec prov q x = match q with
	a::b -> if a = x then false else prov b x
	|[] -> true;;
	
let rec sv a b = match a with
	Var (x,n) -> if prov b x then [(x,n)] else []
	|App (y,z) -> (sv y b) @ (sv z b)
	|Abs (x,y) -> sv y (x::b);;
	
List.iter (fun (x,y) -> print_int y; print_string (x ^ " ")) (sv a []);;
