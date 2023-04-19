let hd = function
	[] -> raise(Failure "hd")
	|h::_ -> h;;
	
let tl = function
	[] -> raise(Failure "tl")
	|_::t -> t;;
	
	
let length l = 
	let rec aux n = function
		[] -> n
		|h::t -> aux (n+1) t
	in aux 0 l;; 
		
		
let rec compare_lengths l ll =
	match l, ll with 
	|[],[] -> 0
	|[], _ -> -1
	|_, [] -> 1
	|_::t1, _::t2 -> compare_lengths t1 t2;;
	
	
let rec nth l n = 
	match l, n with
	|[], _ -> raise(Failure "tl")
	|h::_, 0 -> h
	|h::t, _ -> nth t (n-1);;
	
let rec append l ap =
	match l with
	| [] -> ap
	| h::t -> h::(append t ap);;
	

let rec find f = function
	[] -> raise(Not_found)
	|h::t -> if f h then h else find f t;;
	

let rec for_all p = function
	[] -> true
	| h::t -> p h && for_all p t;;
	
let rec exists f = function
	[] -> false
	| h::t -> if f h then true else exists f t;;

let rec mem a = function 
	[] -> false
	|h::t -> if h = a then true else mem a t;;


let rec rev_append l1 l2 = match l1 with
	[] -> l2
	|h :: t -> rev_append t (h :: l2);;
	
let rec rev l = rev_append l [];;


let filter f l =
	let rec aux acc = function
			[] -> rev acc
			|h::t -> if f h then aux (h::acc) t else aux acc t
	in aux [] l;;


let find_all f l =
	let rec aux acc = function
			[] -> rev acc
			|h::t -> if f h then aux (h::acc) t else aux acc t
	in aux [] l;;

let partition f l =
	let rec aux acc1 acc2 = function
		[] -> (rev acc1, rev acc2)
		|h::t -> if f h then aux (h::acc1) acc2 t else aux acc1 (h::acc2) t
	in aux [] [] l;;

let split l =
	let rec aux acc1 acc2 = function
		[] -> (rev acc1, rev acc2)
		|(h1, h2)::t -> aux  (h1::acc1) (h2::acc2) t
	in aux [] [] l;;
	
let combine l ll=
	let rec aux l ll lo = match l, ll with
		[], [] -> []
		|_,[] | [], _ -> raise(Invalid_argument "Combine")
		|h::t, hh::tt -> (h, hh) :: aux t tt lo
	in aux l ll [];;

let init n f =
	if n < 0 then raise(Invalid_argument "Init")
	else let rec aux l i =
		if i=n then rev l
		else  aux (f i :: l)(i + 1)
	in aux [] 0;;


let rec concat = function
	[] -> []
	|h::t -> append h (concat t);;

let rec flatten = function
	[] -> []
	|h::t -> append h (flatten t);;
	
let map f l =
	let rec aux lo = function
		[] -> []
		|h::t -> (f h):: (aux lo t)
	in aux [] l;;
		
let rev_map f l = 
	let rec aux lo = function
		[] -> lo
		|h::t -> aux ((f h)::lo) t
	in aux [] l;;
	
let map2 f l ll =
	let rec aux l ll lo= 
		match l, ll with
		[],[] -> []
		|_, [] -> raise(Invalid_argument "map2")
		|[], _	-> raise(Invalid_argument "map2")
		|h::t, hh::tt -> (f h hh):: aux t tt lo
	in aux l ll [];;

let rec fold_left f e = function
	[] -> e
	| h::t -> fold_left f (f e h) t;;
	
let rec fold_right f l e = match l with
	[] -> e
	|h::t -> f h (fold_right f t e);;

