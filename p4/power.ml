let rec power x y =
	if y > 0 then x * power x (y-1) else 1;;
	
let rec power' x y =
	if y > 0 && y mod 2 = 0 then power (x*x) (y/2) else 
	if y >0 && y mod 2 != 0 then x * power (x*x) (y/2) else 1;;
