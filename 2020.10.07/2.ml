let p = 11;;
let k = 1023;;
let s = "a";;

let rec hash s n r = if String.length s = 1 then ((int_of_char s.[0]) mod k) else (if n + 1 = (String.length s) then r else
	if n = 0
		then (hash s (n+1) (((int_of_char s.[n])*p + (int_of_char s.[n+1])) mod k))
		else (hash s (n+1) ((r + int_of_char s.[n+1]) mod k)));;
		
let ht = Array.init k (fun x -> []);;

let rec add s ht = let v = ht.(hash s 0 0) in (ht.(hash s 0 0) <- s :: v);;

add s ht;;

Array.iter (fun x -> if x = [] then print_string "" else (List.iter (fun y -> print_string y; print_string " ") x)) ht;;
