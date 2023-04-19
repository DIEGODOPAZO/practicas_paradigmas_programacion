let curry f x y = f(x,y);;

let uncurry f (x,y) = f x y;;

uncurry (+);;
(*- : int * int -> int = <fun>*)

let sum = (uncurry (+));;
(*val sum : int * int -> int = <fun>*)

(*sum 1;;
Type error: la expresion es deberia ser del tipo int * int*)

sum (2,1);;
(*- : int = 3*)

let g = curry (function p -> 2 * fst p + 3 * snd p);;
(*val g : int -> int -> int = <fun>*)

(*g (2,5);;
Type error: la expresion deberia ser tipo int 'a * 'b*)

let h = g 2;;
(*val h : int -> int = <fun>*)

h 1, h 2, h 3;;
(*- : int * int * int = (7, 10, 13)*)



let comp f x y = f (x y);;

let f = let square x = x * x in comp square ((+) 1);;
(*val f : int -> int = <fun>*)

f 1, f 2, f 3;;
(*- : int * int * int = (4, 9, 16)*)



(*para las tres siguientes funciones solo hay una forma de escribirlas, para la cuarta hay infinitas*)


let i a = a;;

let j (a, b) = a;;

let k (a, b) = b;;

let l a = a::[];







