let rec qsort1 ord = function
		[] -> []
		| h::t -> let after, before = List.partition (ord h) t 
	in qsort1 ord before @ h :: qsort1 ord after;;

let rec qsort2 ord =
	let append' l1 l2 = List.rev_append (List.rev l1) l2 in	function
			[] -> []
			| h::t -> let after, before = List.partition (ord h) t 
		in append' (qsort2 ord before) (h :: qsort2 ord after);;

		
let l1 = List.init 1000000 (function x -> Random.int 5000);;


(*Pregunta 1: Esta implementacion no sera buena con listas muy grandes debido a que no es terminal por lo que con una lista lo suficientemente larga dara stack overflow ademas no sera buena en casos en los que la lista este muy desvalanceada*)

(*Pregunta 2: qsort2 presenta varias ventajas respecto a qsort1 como por ejemplo que permite ordenar listas mas largas y es 
mas rapido cuando la lista esta inicialmente ordenada*)

(*Pregunta 3: qsort2 es bastante mas lento que qsort1, aproximadamente un 95% de media cuando la lista esta inicializada aleatoriamente
o cuando esta en orden inverso*)
