open String;;
type prefix = Ques of prefix*prefix | Ans of int;;
let a = Ques(Ques(Ans 1,Ans 2),Ques(Ans 3,Ques(Ans 4,Ans 5)));;

let rec otv a k l = match a with
	|Ques(b,c) -> (otv b k (l ^ "1")) ^ (otv c k (l ^ "0"))
	|Ans d -> if d = k then l else "";;

print_string (otv a 3 "");;
print_string " ";; 

let rec zna a s n = match a with
	|Ques(b,c) ->  if s.[n] = '1' then (zna b s (n+1)) else (zna c s (n+1))
	|Ans d -> d;;

print_int (zna a (otv a 3 "") 0);;

