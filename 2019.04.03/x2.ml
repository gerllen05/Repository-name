let rec funx n s d = if n <= 0 then s else funx (n-1) (s^(String.make d ' ')^"*\n") (d+1) ;;
let rec funy n s = if n <= 0 then s else funy (n-1) (s^(String.make n ' ')^"*\n") ;;
let build n = (funx n "" 0)^(funy (n-2) "") ;;

print_string (build 10);;
