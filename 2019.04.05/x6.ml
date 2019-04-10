type binop = Mult|Plus|Div|Minus;;
type expr = Var of int|Lit of int|Binop of expr*binop*expr;;
let a = read_int();;
let b = read_int();;
let c = read_int();;
let d = read_int();;
let l = Binop(Binop(Var a,Plus,Var b),Mult,Binop(Var c,Plus,Var d));;
let c = 3;;
let oper x = match x with
	Mult -> "*"
	|Plus -> "+"
	|Minus -> "-"
	|Div -> "/";;

let rec write x d = match x with
	Var y -> (string_of_int y)
	|Lit y -> (string_of_int y)
	|Binop(y1,op,y2) -> (String.make (d*c) ' ') ^ (oper op) ^ "\n" ^ (String.make (d*c) ' ') ^ (write y1 (d+1)) ^ "\n" ^ (String.make (d*c) ' ') ^ (write y2 (d+1)) ^ "\n" ;;

print_string ("\n" ^ (write l 0));;
