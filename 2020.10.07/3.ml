let p = 11;;
let k = 1023;;
let s = "a";;
let s1 = "ab";;

let rec hash s n r = if String.length s = 1 then ((int_of_char s.[0]) mod k) else (if n + 1 = (String.length s) then r else
	if n = 0
		then (hash s (n+1) (((int_of_char s.[n])*p + (int_of_char s.[n+1])) mod k))
		else (hash s (n+1) ((r + int_of_char s.[n+1]) mod k)));;
		
let ht = Array.init k (fun x -> []);;

ht.(hash s 0 0) <- [s];;

let rec check_list x l = match l with 
	[] -> false
	|a::b -> if a = x then true else check_list x b;;

let rec check_arr s ht = if (check_list s ht.(hash s 0 0)) then true else false;;

Printf.printf "%b" (check_arr s ht);; 
Printf.printf "%b" (check_arr s1 ht);; 
