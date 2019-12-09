open Array;;
let a = [|[|0;1;1;1;1;1|];
			   [|0;0;1;1;1;1|];
			   [|0;0;0;1;1;1|];
			   [|0;0;0;0;1;1|];
			   [|0;0;0;0;0;1|];
			   [|0;0;0;0;0;0|]|];;

let rec rebro a m k = if (m+1) > length a then 0 else 
	if (k+1) > length a then rebro a (m+1) 0 else 
		if  a.(m).(k) = 1 then 1 + rebro a m (k+1) else rebro a m (k+1);;
	
print_int (rebro a 0 0);;
