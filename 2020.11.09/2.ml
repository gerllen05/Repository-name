type tree = Tree of tree list;;

let l = [Tree [];Tree [Tree []; Tree []]; Tree [Tree [];Tree[]]];;

let rec print l = match l with
	a :: b -> 
		(match a with
		Tree c -> print_string "("; print c; print_string ")");
	print b
	|[] -> print_string "";;

print l;;
