let rec divide l = match l with
	h1::h2::t -> let t1, t2 = divide t in (h1::t1, h2::t2)
	| _ -> l, [];;


let rec merge = function
	[], l | l, [] -> l
	| h1::t1, h2::t2 -> if h1 <= h2 then h1 :: merge (t1, h2::t2)
				else h2 :: merge (h1::t1, t2);;


let rec msort1 l = match l with
		[] | _::[] -> l
		| _ -> let l1, l2 = divide l in
	merge (msort1 l1, msort1 l2);;
	
	
(*Pregunta 1: Si, la no terminalidad de msort1 puede provocar un stack overflow un ejemplo de una lista que lo puede provocar es la siguiente*)

let f x = x  + 1;;
let l2 = List.init 1000000 f;;


let divide' l = let rec aux l1 l2 = function
		[]-> (List.rev l1, List.rev l2)
		|h::[]->(List.rev (h::l1), List.rev l2)
		|h::h2::t -> aux (h::l1) (h2::l2) t
	in aux [] [] l;;
	


let merge' ord (l1, l2) = let rec aux (l, ll) res = 
	match l, ll with
		[], l | l, []-> List.rev_append res l
		|h::t, hh::tt -> if ord h hh then aux (t, hh::tt) (h::res) else aux (h::t, tt) (hh::res)
		
	in aux (l1, l2) [];; 	
	
let rec msort2 ord l = match l with
    [] | _::[] -> l
  | _ -> let l1, l2 = divide' l
         in merge' ord (msort2 ord l1, msort2 ord l2);;
	

	
(*Los tres algoritmos tienen unos tiempos de ejecucion bastantes similares, entre 0.09 y 0.04 segundos dependiendo de la lista,las pruebas de medicion se hicieron en listas inicializadas aleatoriamente con 10000 elementos*)
	
	
	
	
	
	
	
	
	
