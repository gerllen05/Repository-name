type tree = Tree of tree list;;

let l = [Tree [Tree [Tree [];Tree[Tree [];Tree[Tree [];Tree[]]]];Tree[Tree [];Tree[Tree [];Tree[]]]];Tree [Tree [Tree [Tree [];Tree[]];Tree[Tree [];Tree[]]];Tree[Tree [];Tree[]]]];;

let rec check p x = match p with 
	a::b -> if x = a then true else check b x
	|[] ->  false;;
	
let rec sticks p x n = if n<x then  (if (check p n) then "| " ^ (sticks p x (n+2)) else "  " ^ (sticks p x (n+2))) else "";;

let rec niceprint l x p = match l with
	a :: b -> print_string ("*\n" ^ (sticks p x 0) ^ "+-");
		(match a with
		Tree [] -> niceprint [] (x+2) p
		|Tree c -> niceprint c (x+2) (x::p));
	print_string ("\n" ^ (sticks p x 0) ^ "\\-") ; niceprint b (x+2) p
	|[] -> print_string "*";;
	
niceprint l 0 [];;
