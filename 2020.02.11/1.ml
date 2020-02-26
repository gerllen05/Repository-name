let money = 1000;;
let coins = [3;4];;

let rec change n a = 
	if n < 0 then 0 else
	if n = 0 then 1 else
	match a with
		[] -> 0 
		|a1 :: at -> (change (n - a1) a) + (change n at);;

Printf.printf "%d" (change money coins);;
