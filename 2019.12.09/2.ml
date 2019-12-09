open Array;;
let a = [|[|0;1;1;1;1;1|];
			   [|0;0;1;1;1;1|];
			   [|0;0;0;1;1;1|];
			   [|0;0;0;0;1;1|];
			   [|0;0;0;0;0;1|];
			   [|0;0;0;0;0;0|]|];;
			
let rec beg a m k = if m+1 > length a then k::(beg a 0 (k+1)) else
	if k+1 > length a then [] else
			if a.(m).(k) = 0 then (beg a (m+1) k) else (beg a 0 (k+1));;
			
List.iter (fun x -> print_int x;print_string " ") (beg a 0 0);;

