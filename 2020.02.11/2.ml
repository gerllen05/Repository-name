let money = 12;;
let coins = [2;3;4];;
let a = Array.init (List.length coins) (fun x -> Array.init money (fun y -> 0));;

let rec dynamic a y x = 
 a.(y).(x) <- (if x - (List.nth coins y) < -1 then 0 else 
				if x - (List.nth coins y) = -1 then 1 else a.(y).(x - (List.nth coins y))) 
			  +
			  (if y = (List.length coins - 1) then 0 else a.(y+1).(x));
			  (if x = money - 1 then (if y = 0 then a else dynamic a (y-1) x) else if y = 0 then dynamic a (List.length coins - 1) (x+1) else dynamic a (y-1) x);; 

Array.iter (fun x -> Array.iter (fun y -> Printf.printf "%d" y; print_string " ") x; print_string "\n") (dynamic a (List.length coins - 1) 0);;
print_int (a.(0).(money - 1));;














