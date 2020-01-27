open List;;
let l = [1;0;1;-1];;

let rec tos l = match l with
	a::b -> if a = (-1) then "$" ^ tos (tl l) else (string_of_int (hd l)) ^ tos (tl l)
	|[] -> "";;

print_string  (tos l);;


