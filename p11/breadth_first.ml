open G_tree;;

let rec breadth_first = function
    Gt (x, []) -> [x]
  | Gt (x, (Gt (y, t2))::t1) -> x :: breadth_first (Gt (y, t1@t2));;



let rec breadth_first_t gtr =
  let rec aux l level next = 
    match(level, next) with
    [],[] -> List.rev l
    |[], next -> aux l (List.rev next) []
    |Gt(x,t2)::t1, next -> aux (x::l) t1 (List.rev_append t2 next)
  in aux [] [gtr] [];;



let t2 = Gt(0, List.init 3000000 (function v -> Gt(v, [])));;


