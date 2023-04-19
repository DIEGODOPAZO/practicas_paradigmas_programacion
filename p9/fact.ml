let rec fact = function 0->1 | n-> n * fact(n - 1);;

if Array.length Sys.argv = 2
	then 
	
	let x = 
		try 
			int_of_string(Sys.argv.(1)) ;
		with
			_ ->print_endline("fact: argumento invalido");
			exit 1
			
	in
		if x >= 0 then 			
			print_endline(string_of_int(fact(x)))
		else print_endline("fact: argumento invalido")
else print_endline ("fact: numero de argumentos inválidos")		

