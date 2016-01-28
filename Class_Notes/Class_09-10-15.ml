(*Problem 1:

all : bool list -> bool
*)

let rec all (lst : bool list) : bool = 
	match lst with
	| [] -> true
	| hd::tl when hd = true -> all(tl)
	| _ -> false

let _ = assert (all [] = true)
let _ = assert (all [true;false] = false)
let _ = assert (all [true;true;true] = true)

(*
	Problem 2:

	ints: int -> int list

*)

let fastRev (l : 'a list) : 'a list =
  let rec revHelper (remain, sofar) =
    match remain with
    | [] -> sofar
    | hd::tl -> revHelper (tl, hd :: sofar)
in revHelper(l, [])

let rec ints (num : int) : int list = 
	match num with
	| low when low < 1 -> []
	| _ -> fastRev (num :: ints (num - 1))

let _ = assert (ints 2 = [1;2])
let _ = assert (ints 0 = [])

(* Matt's Version of Ints *)

let ints n = 
	let rec helper i = 
	if i > n
	then []
	else i :: helper(i + 1)
in helper 1


(*
	Problem 3

	nth : (int * 'a list) -> 'a option

	nth (0, [1;2;3]) = Some 1
	nth (2, [1;2;3]) = Some 3
	nth (3, [1;2;3]) = None
*)

type 'a option = None | Some of 'a

let rec nth ((n, l) : (int * 'a list)) : 'a option = 
	match (n, l) with
	| (_, []) -> None
	| (0, hd::_) -> Some hd
	| (i, _::tl) -> nth (i - 1, tl)

let _ = assert (nth (0, [1;2;3]) = Some 1)  
let _ = assert (nth (2, [1;2;3]) = Some 3)
let _ = assert (nth (0, []) = None)
let _ = assert (nth (3, [1;2;3]) = None)      