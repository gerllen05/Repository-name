open String;;

let p = Sys.argv.(1);;
let q = Sys.argv.(2);;

let f = open_in p;;
let g = open_out q;;

let rec st n =
	try 
		let x = input_line f  in (if length x > length n then st x else st n) 
	with End_of_file -> n;;

let k = st "";;

close_in f;;
let f = open_in p;;

let rec copst()= 
	try 
		let x = input_line f  in (if x = k then (copst()) else (x ^ "\n" ^ (copst())))  
	with End_of_file -> "";;
	
	
let copyst() = output_string g (copst());;

copyst();;
