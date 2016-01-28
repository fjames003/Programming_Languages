let foo x = 7 + x;;
assert(foo 3 = 10);;
assert(foo 12 = 19);;

let bar y = foo y + 1;;

(*
	Recursive datatypes:
		-Great for defining ifinite sets of values.
		-Containers: lists, sets, key-value maps
	Lists need not be built in!
*)

type intList = Empty | Node of int * intList;;

(* 
Empty ~ []
Node ~ ::

[1;2] = 1::2::[] ~ Node (1, Node (2, Empty)) 
*)

let rec sumList l = 
	match l with
	| Empty -> 0
	| Node(x, xs) -> x + sumList xs
;;

type 'a myList = Empty | Node of ('a * 'a myList);;

Node("hi", Empty);;
Node(1, Empty);;

type 'a append_list = 
		Empty
	| Single of 'a 
	| Append of ('a append_list * 'a append_list)
	;;

let rec sumList l = 
 match l with
 | Empty -> 0
 | Single x -> x
 | Append(l1, l2) -> sumList l1 + sumList l2
 ;;

 let suml = Append (Single 5, Single 6);;  

 sumList suml

 let l1 = Single 1;;
 let l1' = Append(Single 1, Empty);;

 (* let l123 = Append(Append(Single 1 , Single 2), Single 3));;
 let l123' = Append(Single 1 , Single 2, Append(Single 3));;
 *)
 let rec equal ((l1, l2) : 'a append_list * 'a append_list) : bool = 
	let rec to_list (l : 'a append_list) : 'a list = 
		match l with
		| Empty -> []
		| Single e -> [e]
		| Append(l1, l2) -> to_list l1 @ to_list l2
	in to_list l1 = to_list l2
;;

type tree = Leaf
			| TreeNode of tree * int * tree
;;

let t1 = TreeNode(Leaf, 1, Leaf);;
let t3 = TreeNode (Leaf, 3, Leaf);;
let t123 = TreeNode (t1, 2, t3);;

let rec size (t:tree) : int = 
	match t with
	| Leaf -> 0
	| TreeNode(left, _, right) -> 1 + size left + size right
;;

let _ = assert(size t3 = 1);;
let _ = assert(size Leaf = 0);;
let _ = assert(size t123 = 3);;

(*
WORK_ON:
	tree_to_list Leaf = [];;
	tree_to_list (TreeNode (Leaf, 1, Leaf)) = [1];;
	tree_to_list (TreeNode (t123, 4, t567)) = [1;2;3;4;5;6;7];;
*)
