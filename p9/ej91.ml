let to0from n = 
	let rec aux n2 l =
		if n >= n2 then aux (n2+1) (n2::l) else l
	in aux 0 [];;			
		
let fromto m n = 
	let rec aux n2 l=
		 if m > n2 then l else aux (n2-1) (n2::l)
	in aux n [];;
	
let incseg l = 
	let rec aux n lo = function
		[] -> List.rev lo
		| h::t -> aux (h+n) ((h+n)::lo) t
	in aux 0 [] l;; 
	
let remove x l = 
	let rec aux lo = function
		[] -> List.rev lo
		|h::t -> if h = x then List.rev_append lo t else aux (h::lo) t 
	in aux [] l;;
	
let compress l =
	let rec aux lo = function
		[] -> List.rev lo
		|h::hh::t -> if h = hh then aux lo (hh::t) else aux (h::lo)(hh::t)
		|h::[]-> List.rev (h::lo)
	in aux [] l;;

