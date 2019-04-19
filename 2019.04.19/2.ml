type tree = Leaf | Node of int*tree*tree;;
let n = read_int();;
let rec bamb k = if k < n then Node(k,Leaf,bamb (k+1)) else Node (k,Leaf,Leaf);;

