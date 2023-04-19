
open Bin_tree;;

let rec insert_tree ord x = function
	Empty -> Node(x, Empty, Empty)
	|Node(xx, l, r) -> if ord x xx then Node(xx, insert_tree ord x l, r) else Node(xx, l, insert_tree ord x r) ;;

let tsort ord l =
  inorder (List.fold_left (fun a x -> insert_tree ord x a) Empty l)
;;


