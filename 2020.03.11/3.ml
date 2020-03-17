type l = Var of string * int | App of l * l | Abs of string * l;;
let a = App (Var ("x",2),Abs ("z",Var ("z",9)));;
let d = "y"
let c = "x";;

let rec change a d c = match a with
	Var (s,n) -> if s = d then Var (c,n) else Var (s,n)
	|App (y,z) -> App (change y d c,change z d c)
	|Abs (x,y) -> if x = d then Abs (x,y) else Abs (x,change y d c);;

let rec prov q x = match q with
	a::b -> if a = x then false else prov b x
	|[] -> true;;

let rec sv a b = match a with
	Var (x,n) -> if prov b x then [(x,n)] else []
	|App (y,z) -> (sv y b) @ (sv z b)
	|Abs (x,y) -> sv y (x::b);;
	
let rec svchan a d c = match (sv a []) with
	(x,n)::t -> if prov (sv (change a d c) []) (x,n) then Printf.printf "%b" false else svchan t d c
	|[] -> Printf.printf "%b" true;;
	

