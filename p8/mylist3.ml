let rec remove a = function
	[] -> []
	|h::t -> if h = a then t else h::(remove a t);;
	
let rec remove_all a = function
	[] -> []
	|h::t -> if h = a then remove_all a t else h::remove_all a t;;
	
let rec ldif l1 l2 = match l1, l2 with
	_, [] | [], _ -> l1
	|h::t, hh::tt -> ldif (remove_all hh l1) tt;;
	
let lprod l1 l2 = 
	let rec aux l ll lo = match l, ll with
		[], _ -> List.rev lo
		|h::t, [] -> aux t l2 lo
		|h::t, hh::tt -> aux (h::t) tt ((h, hh)::lo)
	in aux l1 l2 [];;

let divide l = 
	let rec aux cont (l1, l2) = function
		[] -> (List.rev l1, List.rev l2)
		|h::t -> if (cont mod 2) = 1 then aux (cont+1) (h::l1, l2) t else  aux (cont+1) (l1, h::l2) t
	in aux 1 ([], []) l;;
