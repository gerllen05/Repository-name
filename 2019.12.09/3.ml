open Array;;
let a = [|[|0;1;1;1;1;1|];
			   [|0;0;1;1;1;1|];
			   [|0;0;0;1;1;1|];
			   [|0;0;0;0;1;1|];
			   [|0;0;0;0;0;1|];
			   [|0;0;0;0;0;0|]|];;
			   
let rec out a m k = if k+1 > length a then 0 else
			if a.(m).(k) = 1 then 1 + (out a m (k+1)) else (out a m (k+1));;
			
let rec tin a m k = if m+1 > length a then 0 else
			if a.(m).(k) = 1 then 1 + (tin a (m+1) k) else (tin a (m+1) k);;
			
let rec l n = if n+1 > length a then [] else (n,out a n 0,tin a 0 n) :: (l (n+1));;

List.iter (fun (x,y,z) -> print_string "(";print_int x;print_string ",";print_int y;print_string ",";print_int z;print_string ") ") (l 0);;

			   

