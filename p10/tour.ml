let notmem l e =
  not (List.mem e l);;

let is_in_board m n (x,y) =
  x >= 1 && x <= m && y >= 1 && y <= n;;

let moves m n (x,y) c =
 let rec aux a = if(a<>0) then [x,y-a; x-a,y; x,y+a; x+a,y;] @ aux (a-1) else []
    in aux c;;
   
let legal_moves m n (x,y) visited tree c =
	List.filter (fun x -> List.mem x tree && notmem visited x) (moves m n (x,y) c);;
 
let tour m n tree c =
	let rec aux solution l = match l with
          [] -> raise Not_found
        | (h1, h2)::t -> if ((h1,h2)=(m,n)) then List.rev ((h1,h2)::solution)
                  else try aux ((h1,h2)::solution) (legal_moves m n (h1,h2) solution tree c)
                       with Not_found -> aux solution t
    in if List.mem (1,1) tree then aux [] [(1,1)]
     else raise Not_found;;
