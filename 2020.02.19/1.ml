let a = [|[|false;false;false;false;true |];
		  [|false;false;false;true ;false|];
		  [|false;false;false;false;false|];
		  [|false;true ;false;false;false|];
		  [|true ;false;true ;false;false|]|];;
let l = Array.length a.(0);;
let n = Array.length a;;

let rec ft a y x =
if x = (l+1) then false else
if a.(y).(x+1) then true
else a.(y).(x+2);;

let rec stairs a y x =
if x = l || y = (-1) then a else match a.(y).(x) with
|true -> if ft a y x then stairs a y (x+1) else stairs a (y-1) (x+1)
|false -> (a.(y).(x) <- true ; if ft a y x then stairs a y (x+1) else stairs a (y-1) (x+1));;

Array.iter (fun x -> Array.iter (fun y -> print_int (if y then 1 else 0)) x; print_string "\n") (stairs a (n-1) 0);;
