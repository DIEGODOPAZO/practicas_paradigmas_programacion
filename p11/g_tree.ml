type 'a g_tree =
  Gt of 'a * 'a g_tree list
;;

let rec size = function 
    Gt (_, []) -> 1
  | Gt (r, h::t) -> size h + size (Gt (r, t))
;;

let rec height = function 	 
	Gt (_, []) -> 1
	|Gt (r, l) -> 1 + List.fold_left (fun acc child -> max acc (height child)) 0 l;;
	
let rec leaves = function
	Gt (x, []) -> [x]
	|Gt(r, l) -> List.concat(List.map leaves l);;
	
let rec mirror (Gt(v, l)) =
	Gt(v, List.rev(List.map mirror l));;
	
let rec preorder = function
	Gt(x, []) -> [x]
	|Gt(r, l) -> [r] @ List.concat(List.map preorder l);;

let rec postorder = function
	Gt(x, []) -> [x]
	|Gt(r, l) -> List.concat(List.map postorder l) @ [r];;
