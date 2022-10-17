let rec sum_cifras n =
	if n < 10 then n else n mod 10 + sum_cifras(n/10);;
	
	
let rec num_cifras n =
	if abs(n) < 10 then 1 else 1 + num_cifras(n/10);;	

let rec exp10 n = 
	if n = 0 then 1 else 10 * exp10 (n-1);;
	
(*
let reverse n =
	let rev = 0 in
	let rec inver n = 
		if n < 10 then n else rev + (n mod 10) * exp10(num_cifras(n) - 1) + inver (n/10) in inver n;;
*)

let rec reverse n =
	let rev = 0 in
	if n < 10 then n else rev + (n mod 10) * exp10(num_cifras(n) - 1) + reverse (n/10);;
		

let rec palindromo s =
	if String.length s = 0 || String.length s = 1 then true else if s.[0] = s.[String.length s - 1] then 
	palindromo (String.sub s 1 (String.length s - 2)) else false;;


