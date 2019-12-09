let l = [1;0;1;1;1;];;
let o = read_int();;

let rec trans l o p = match l with
	[] -> 0
	|a::b -> (a * p) + (trans b o (p * o));;

let rec trback n o p = if p < 1 then [] else ((n-(n mod p))/p) :: (trback (n mod p) o (p/o));;
let rec find n o p = if p > n then trback n o (p / o) else find n o (p * o);;

List.iter (fun x -> print_int x;print_string " ") (List.rev (find (trans l o 1) o 1));;
print_string "                ";;
List.iter (fun x -> print_int x;print_string " ") (List.rev (find 512 o 1));;
