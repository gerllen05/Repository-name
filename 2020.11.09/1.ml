type tree = Tree of tree list;;

let s = "(((()()()())()()))";; 

let rec parse n = 
	if n >= (String.length s) then ([],n) else 
		match s.[n] with 
			|'(' -> let (tl,n) = parse (n+1) in 
				if s.[n] = ')' then let (tl2,n) = parse (n+1) in 
					(Tree tl :: tl2,n) else failwith"" 
			|')' -> ([],n) 
			|_ -> failwith"";; 

fst (parse 0);;
