open List;;

type prefix = Ques of prefix*prefix | Ans of int;;
let a = Ques(Ques(Ans 1,Ans 2),Ques(Ans 3,Ques(Ans 4,Ans 5)));;

let rec allzna a = match a with
	|Ques(b,c) -> (allzna b) @ (allzna c)
	|Ans d -> [d];;
let b = allzna a;;
List.iter (fun x -> print_int x) b;;
print_string " ";;

let rec otv a k l = match a with
	|Ques(b,c) -> (otv b k (l @ [1])) @ (otv c k (l @ [0]))
	|Ans d -> if d = k then l else [];;

let rec voc a b = if (length b) = 0 then [] else (hd b,otv a (hd b) []) :: (voc a (tl b));;
 
List.iter (fun (x,y) -> print_string "(" ; print_int x ; print_string "," ; List.iter (fun x -> print_int x) y ; print_string ")") (voc a b);;
