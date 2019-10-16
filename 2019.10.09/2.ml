let p = Sys.argv.(1);;
let q = Sys.argv.(2);;

let f = open_in_bin p;;
let g = open_out_bin q;;

let rec copbin()= 
	try 
		let x = input_line f  in (x ^ "\n" ^ (copbin())) 
	with End_of_file -> "";;
	
let copybin() = output_string g (copbin());;

copybin();;
