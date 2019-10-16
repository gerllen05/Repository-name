let p = Sys.argv.(1);;
let q = Sys.argv.(2);;

let f = open_in p;;
let g = open_out q;;

let rec cop()= 
	try 
		let x = input_line f  in (x ^ "\n" ^ (cop())) 
	with End_of_file -> "";;
	
let copy() = output_string g (cop());;

copy();;

 
